return {
  "mfussenegger/nvim-lint",
  event = { "BufReadPost", "BufWritePost", "InsertLeave" },
  config = function()
    local lint = require("lint")
    -- LSP servers cover the rest; nvim-lint only adds linters they lack
    lint.linters_by_ft = {
      python = { "mypy" },
      html = { "tidy" },
      sh = { "shellcheck" },
    }

    local group = vim.api.nvim_create_augroup("nvim_lint", { clear = true })
    vim.api.nvim_create_autocmd({ "BufWritePost", "BufReadPost", "InsertLeave" }, {
      group = group,
      callback = function()
        lint.try_lint()
      end,
    })
  end,
}
