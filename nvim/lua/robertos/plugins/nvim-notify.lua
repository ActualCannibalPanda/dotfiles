return {
  'rcarriga/nvim-notify',
  opts = {
    background_color = '#4444BB',
  },
  keys = {
    {
      '<leader>n?',
      '<cmd>Telescope notify<cr>',
      desc = 'Notfiy History',
    },
  },
}
