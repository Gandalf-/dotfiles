return {
  "mfussenegger/nvim-lint",
  event = { "BufReadPost", "BufWritePost", "InsertLeave" },
  config = function()
    local lint = require("lint")

    lint.linters.ty = {
      cmd = "ty",
      args = { "check", "--output-format", "concise" },
      stdin = false,
      append_fname = true,
      ignore_exitcode = true,
      stream = "stdout",
      parser = require("lint.parser").from_pattern(
        "([^:]+):(%d+):(%d+): (%a+)%[([%a-]+)%] (.*)",
        { "file", "lnum", "col", "severity", "code", "message" },
        {
          error = vim.diagnostic.severity.ERROR,
          warning = vim.diagnostic.severity.WARN,
        },
        { source = "ty" }
      ),
    }

    -- LSP servers cover the rest; nvim-lint only adds linters they lack
    local wanted = {
      python = { "ty" },
      html = { "tidy" },
      sh = { "shellcheck" },
    }
    local by_ft = {}
    for ft, linters in pairs(wanted) do
      local present = {}
      for _, name in ipairs(linters) do
        if vim.fn.executable(name) == 1 then
          present[#present + 1] = name
        end
      end
      if #present > 0 then
        by_ft[ft] = present
      end
    end
    lint.linters_by_ft = by_ft

    local group = vim.api.nvim_create_augroup("nvim_lint", { clear = true })

    -- all linters on read/write
    vim.api.nvim_create_autocmd({ "BufWritePost", "BufReadPost" }, {
      group = group,
      callback = function()
        lint.try_lint()
      end,
    })

    -- on insert-leave, skip ty: it reads the on-disk file (stdin=false), so on an
    -- unsaved buffer it lints stale content. stdin-based linters (shellcheck,
    -- tidy) see the buffer itself, so they still run.
    vim.api.nvim_create_autocmd("InsertLeave", {
      group = group,
      callback = function()
        lint.try_lint(nil, { filter = function(l) return l.name ~= "ty" end })
      end,
    })

    -- lint the buffer that triggered loading: the autocmd above is registered
    -- mid-event, so it won't fire for the current buffer until the next event.
    lint.try_lint()
  end,
}
