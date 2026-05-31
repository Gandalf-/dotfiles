return {
  "nvim-treesitter/nvim-treesitter",
  -- master branch: the default `main` branch is a rewrite with an incompatible
  -- setup API and no `nvim-treesitter.configs`
  branch = "master",
  build = ":TSUpdate",
  event = { "BufReadPost", "BufNewFile" },
  main = "nvim-treesitter.configs",
  opts = {
    ensure_installed = {
      "bash", "c", "cpp", "go", "haskell", "html", "javascript",
      "json", "lua", "markdown", "python", "rust", "toml", "vim", "vimdoc", "yaml",
    },
    auto_install = true,
    highlight = { enable = true },
    indent = { enable = true },
  },
}
