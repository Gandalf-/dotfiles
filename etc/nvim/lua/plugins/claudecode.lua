return {
  -- Native Claude Code IDE integration over a localhost WebSocket (same
  -- protocol as the VS Code extension). nvim runs the IDE server; you launch
  -- `claude` yourself in a tmux pane and connect it with `/ide`.
  --
  -- Workflow:
  --   1. Open nvim  -> server auto-starts, writes ~/.claude/ide/<port>.lock
  --   2. In the CC pane: `claude`, then `/ide`, pick "Neovim" (matched by cwd)
  --   3. Visual-select -> <leader>as sends an @file#Lstart-end reference to CC
  --   4. Tab to the CC pane (vim-tmux-navigator) and type your question
  {
    "coder/claudecode.nvim",
    -- VeryLazy (not on keypress) so the server is up the moment you decide to
    -- /ide-attach -- including attaching nvim to an already-running session.
    event = "VeryLazy",
    opts = {
      -- "none": nvim manages only the IDE server, never the terminal. You own
      -- launching claude (or not). Pairs 1:1 per nvim via the lockfile's port.
      terminal = { provider = "none" },
    },
    keys = {
      { "<leader>as", "<cmd>ClaudeCodeSend<cr>", mode = "v", desc = "Send selection to Claude" },
      { "<leader>aa", "<cmd>ClaudeCodeAdd %<cr>", desc = "Add current file to Claude" },
      { "<leader>a?", "<cmd>ClaudeCodeStatus<cr>", desc = "Claude IDE status" },
    },
    cmd = { "ClaudeCodeSend", "ClaudeCodeAdd", "ClaudeCodeStatus", "ClaudeCodeStart", "ClaudeCodeStop" },
  },
}
