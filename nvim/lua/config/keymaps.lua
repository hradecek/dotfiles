local keymap = vim.keymap
local opts = { noremap = true, silent = true }

-- <ESC>
keymap.set("i", "jj", "<ESC>")
-- Increment & decrement
keymap.set("n", "+", "<C-a>")
keymap.set("n", "-", "<C-x>")
-- Select all
keymap.set("n", "<C-a>", "gg<S-v>G")
-- Jumplist
keymap.set("n", "<C-m>", "<C-i>", opts)
-- New tab
keymap.set("n", "te", "tabedit", opts)
-- Next & Prev tab
keymap.set("n", "<tab>", ":tabnext<Return>", opts)
keymap.set("n", "<s-tab>", ":tabprev<Return>", opts)
-- Page up & down
keymap.set("n", "<s-k>", "<C-b>")
keymap.set("n", "<s-j>", "<C-f>")

