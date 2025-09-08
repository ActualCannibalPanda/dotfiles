return {
  'rcarriga/nvim-notify',
  keys = {
    {
      '<leader>n?',
      function()
        require('notify').history()
      end,
      desc = 'Notfiy History',
    },
  },
}
