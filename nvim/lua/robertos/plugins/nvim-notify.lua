return {
  'rcarriga/nvim-notify',
  opts = {
    background_colour = '#000000',
  },
  keys = {
    {
      '<leader>n?',
      '<cmd>Telescope notify<cr>',
      desc = 'Notfiy History',
    },
  },
}
