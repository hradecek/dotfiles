-- Shorten timeout for leaving from INSERT mode
vim.api.nvim_create_autocmd("InsertLeave", {
  command = "set timeoutlen=100",
})

-- Turn paste mode when leaving insert
vim.api.nvim_create_autocmd("InsertLeave", {
  pattern = "*",
  command = "set nopaste",
})

-- Fix conceallevel for JSON files
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "json", "jsonc" },
  callback = function()
    vim.wo.spell = false
    vim.wo.conceallevel = 0
  end,
})

