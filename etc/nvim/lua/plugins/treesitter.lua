return {
  "nvim-treesitter/nvim-treesitter",
  branch = "main", -- rewrite targeting Neovim 0.12+; master is frozen at 0.11
  lazy = false,    -- main branch does not support lazy-loading
  -- main builds every parser by shelling out to the `tree-sitter` CLI. Where
  -- that binary is absent (e.g. FreeBSD, which has no pkg for the Rust CLI),
  -- run :TSUpdate only when it exists so updates don't error.
  build = function()
    if vim.fn.executable("tree-sitter") == 1 then
      vim.cmd("TSUpdate")
    end
  end,
  config = function()
    -- Parsers to install/build (replaces master's `ensure_installed`).
    -- `markdown_inline` is needed for fenced-code highlighting; `fish` added
    -- since it's the shell (master pulled it via auto_install).
    local langs = {
      "bash", "c", "cpp", "fish", "go", "haskell", "html", "javascript",
      "json", "lua", "make", "markdown", "markdown_inline", "python",
      "rust", "toml", "vim", "vimdoc", "yaml",
    }
    -- install() compiles each parser with the `tree-sitter` CLI; without it
    -- every call errors loudly. Only build when the CLI is on $PATH. Parsers
    -- already built here, plus the ones neovim bundles (c, lua, markdown,
    -- markdown_inline, query, vim, vimdoc), keep working regardless; the
    -- FileType handler below degrades to vim's regex syntax for the rest.
    if vim.fn.executable("tree-sitter") == 1 then
      require("nvim-treesitter").install(langs) -- async; no-op if already built
    end

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
