return {
  'catppuccin/nvim',
  event = 'VeryLazy',
  name = 'catppuccin',
  config = function()
    require('catppuccin').setup({
      flavour = 'mocha',
    })
    vim.cmd.colorscheme('catppuccin')
  end,
}
