return {
  "junegunn/goyo.vim",
  config = function()
    vim.api.nvim_set_keymap('n', '<leader>p', ':Goyo<CR>', { noremap = true, silent = true })
  end
}
