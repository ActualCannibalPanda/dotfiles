return {
  'S1M0N38/love2d.nvim',
  event = 'VeryLazy',
  version = '2.*',
  opts = {},
  keys = {
    {
      '<leader>v',
      ft = 'lua',
      desc = 'Love2D',
    },
    {
      '<leader>vv',
      '<cmd>LoveRun<cr>',
      ft = 'lua',
      desc = 'Run Love2D',
    },
    {
      '<leader>vs',
      '<cmd>LoveStop<cr>',
      ft = 'lua',
      desc = 'Stop Love2D',
    },
  },
}
