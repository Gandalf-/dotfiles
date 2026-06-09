return {
  "mfussenegger/nvim-lint",
  event = { "BufReadPost", "BufWritePost", "InsertLeave" },
  config = function()
    local lint = require("lint")
    -- LSP servers cover the rest; nvim-lint only adds linters they lack.
    -- Skip any whose binary isn't installed -- otherwise try_lint notifies a
    -- warning on every read/write/insert-leave for that filetype. (For these
    -- three the linter name matches its executable.)
    local wanted = {
      python = { "mypy" },
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

    -- on insert-leave, skip mypy: it's slow and checks the on-disk file, so
    -- running it on every unsaved edit just lags. fast linters still run.
    vim.api.nvim_create_autocmd("InsertLeave", {
      group = group,
      callback = function()
        lint.try_lint(nil, { filter = function(l) return l.name ~= "mypy" end })
      end,
    })

    -- lint the buffer that triggered loading: the autocmd above is registered
    -- mid-event, so it won't fire for the current buffer until the next event.
    lint.try_lint()
  end,
}
