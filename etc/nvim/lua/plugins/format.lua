return {
  {
    "stevearc/conform.nvim",
    event = { "BufWritePre" },
    cmd = { "ConformInfo" },
    opts = {
      formatters_by_ft = {
        python = { "ruff_fix", "ruff_format" },
        go = { "gofmt" },
        rust = { "rustfmt" },
        c = { "clang_format" },
        cpp = { "clang_format" },
        html = { "prettier" },
        javascript = { "prettier" },
        ["_"] = { "trim_whitespace" }, -- every other filetype
      },
      formatters = {
        prettier = {
          prepend_args = { "--single-quote", "--tab-width", "2", "--print-width", "100" },
        },
      },
      format_on_save = function(bufnr)
        -- :FormatToggle flips these
        if vim.b[bufnr].disable_autoformat or vim.g.disable_autoformat then
          return
        end
        return { timeout_ms = 1000, lsp_format = "fallback" }
      end,
    },
    init = function()
      vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
      vim.api.nvim_create_user_command("Format", function()
        require("conform").format({ async = true, lsp_format = "fallback" })
      end, { desc = "Format buffer" })
      vim.api.nvim_create_user_command("FormatToggle", function(args)
        if args.bang then
          vim.b.disable_autoformat = not vim.b.disable_autoformat
        else
          vim.g.disable_autoformat = not vim.g.disable_autoformat
        end
      end, { bang = true, desc = "Toggle format-on-save" })
    end,
  },
}
