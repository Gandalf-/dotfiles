return {
  "nvim-treesitter/nvim-treesitter",
  branch = "main", -- rewrite targeting Neovim 0.12+; master is frozen at 0.11
  lazy = false,    -- main branch does not support lazy-loading
  build = ":TSUpdate",
  config = function()
    -- Parsers to install/build (replaces master's `ensure_installed`).
    -- `markdown_inline` is needed for fenced-code highlighting; `fish` added
    -- since it's the shell (master pulled it via auto_install).
    local langs = {
      "bash", "c", "cpp", "fish", "go", "haskell", "html", "javascript",
      "json", "lua", "make", "markdown", "markdown_inline", "python",
      "rust", "toml", "vim", "vimdoc", "yaml",
    }
    require("nvim-treesitter").install(langs) -- async; no-op if already built

    -- Open files unfolded (treesitter foldexpr otherwise folds aggressively).
    vim.o.foldlevelstart = 99

    -- Enable highlighting + folding per buffer. main does NOT auto-enable.
    vim.api.nvim_create_autocmd("FileType", {
      callback = function(args)
        -- Require a highlights query, not just a parser. A stale parser with
        -- no query would start treesitter, suppress `syntax on`, and apply
        -- nothing -- blanking the buffer. Filetypes without a query fall
        -- through to vim's built-in regex highlighting.
        local lang = vim.treesitter.language.get_lang(args.match) or args.match
        if not vim.treesitter.query.get(lang, "highlights") then
          return
        end
        if pcall(vim.treesitter.start, args.buf) then
          vim.wo[0][0].foldmethod = "expr"
          vim.wo[0][0].foldexpr = "v:lua.vim.treesitter.foldexpr()"
        end
      end,
    })
  end,
}
