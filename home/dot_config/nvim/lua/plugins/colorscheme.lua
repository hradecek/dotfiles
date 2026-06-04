return {
  "craftzdog/solarized-osaka.nvim",
  lazy = true,
  priority = 1000,
  opts = function()
    return {
      transparent = true,
      terminal_colors = true,
      on_highlights = function(highlights, colors)
        highlights.Whitespace = { fg = colors.base02 }
      end
    }
  end
}
