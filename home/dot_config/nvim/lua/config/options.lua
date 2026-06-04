-- Undercurl
vim.cmd([[let &t_Cs = "\e[4:3m]"]])

-- Whitespaces
vim.opt.listchars = {
  tab = ">—",
  extends = "»",
  precedes = "«",
  trail = "·",
  space = "·",
}

-- Turn off autoformat
vim.g.autoformat = false

-- Tab width
vim.opt.tabstop = 4

