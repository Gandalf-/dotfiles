-- language servers are expected on $PATH; ones that aren't installed are skipped.
-- haskell is driven by haskell-tools.nvim, not lspconfig.

-- Silence benign LSP request failures. Inlay hints (and code lenses) re-fire on
-- every edit; on files HLS can't typecheck as a module -- the hspec-discover
-- Spec.hs stub, .cabal files -- the request fails with -32803 (RequestFailed)
-- every time. Neovim's generic handler notifies on each failure, flooding the
-- message line and tripping the hit-enter prompt that swallows keystrokes (the
-- "can't edit" symptom). Drop just that one benign code, exactly as neovim
-- already drops ContentModified upstream; successful requests still render,
-- real errors still surface.
for _, method in ipairs({ "textDocument/inlayHint", "textDocument/codeLens" }) do
  local orig = vim.lsp.handlers[method]
  if orig then
    vim.lsp.handlers[method] = function(err, result, ctx, config)
      if err and err.code == -32803 then
        return
      end
      return orig(err, result, ctx, config)
    end
  end
end

-- buffer-local maps for any attached server
local function on_attach(_, bufnr)
  local function nmap(lhs, rhs, desc)
    vim.keymap.set("n", lhs, rhs, { buffer = bufnr, silent = true, desc = desc })
  end

  nmap("K", vim.lsp.buf.hover, "Hover")
  nmap("<leader>gd", vim.lsp.buf.definition, "Go to definition")
  nmap("<leader>gr", vim.lsp.buf.references, "References")
  nmap("<leader>gs", vim.lsp.buf.workspace_symbol, "Workspace symbols")
  nmap("<leader>ca", vim.lsp.buf.code_action, "Code action")
  nmap("<leader>rn", vim.lsp.buf.rename, "Rename")
  nmap("<leader>cl", vim.lsp.codelens.run, "Run codelens")

  pcall(vim.lsp.inlay_hint.enable, true, { bufnr = bufnr })
end

return {
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      vim.diagnostic.config({
        -- message shown below the current line; swap for `virtual_text = true`
        -- if the line-shift as the cursor moves is distracting
        virtual_lines = { current_line = true },
        underline = true,
        severity_sort = true,
        signs = {
          text = {
            [vim.diagnostic.severity.ERROR] = "X",
            [vim.diagnostic.severity.WARN] = ">",
            [vim.diagnostic.severity.INFO] = ">",
            [vim.diagnostic.severity.HINT] = ">",
          },
        },
      })

      vim.keymap.set("n", "<leader>a", function() vim.diagnostic.jump({ count = 1, float = false }) end,
        { silent = true, desc = "Next diagnostic" })
      vim.keymap.set("n", "<C-a>", vim.diagnostic.open_float,
        { silent = true, desc = "Show diagnostic detail" })

      vim.api.nvim_create_autocmd("LspAttach", {
        callback = function(args)
          local client = vim.lsp.get_client_by_id(args.data.client_id)
          on_attach(client, args.buf)
        end,
      })

      -- server name -> binary to probe before enabling.
      -- python: ruff (lint + format) here, mypy via nvim-lint; no pyright.
      local servers = {
        ruff = "ruff",
        gopls = "gopls",
        rust_analyzer = "rust-analyzer",
        clangd = "clangd",
      }
      local enable = {}
      for name, bin in pairs(servers) do
        if vim.fn.executable(bin) == 1 then
          enable[#enable + 1] = name
        end
      end
      if #enable > 0 then
        vim.lsp.enable(enable)
      end
    end,
  },

  {
    "mrcjkb/haskell-tools.nvim",
    version = "^6",
    ft = { "haskell", "lhaskell", "cabal", "cabalproject" },
    lazy = false,
  },
}
