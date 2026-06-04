local keymap = vim.keymap
local opts = { noremap = true, silent = true }

-- <ESC>
keymap.set("i", "jj", "<ESC>")
-- Increment & decrement
keymap.set("n", "+", "<C-a>")
keymap.set("n", "-", "<C-x>")
-- Select all
keymap.set("n", "<C-a>", "gg<S-v>G")

