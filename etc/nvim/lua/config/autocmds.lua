local ui = vim.api.nvim_create_augroup("ui_commands", { clear = true })

-- equalize splits when the window is resized
vim.api.nvim_create_autocmd("VimResized", {
  group = ui,
  command = "wincmd =",
})

-- cursorline only outside insert mode
vim.api.nvim_create_autocmd({ "InsertEnter", "InsertLeave" }, {
  group = ui,
  command = "set cursorline!",
})
