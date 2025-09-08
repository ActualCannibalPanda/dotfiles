return {
  'folke/which-key.nvim',
  event = 'VeryLazy',
  opts_extended = { 'spec' },
  opts = {
    spec = {
      {
        mode = { 'n', 'v' },
        { '[', group = 'prev' },
        { ']', group = 'next' },
      },
    },
  },
  keys = {
    {
      '<leader>?',
      function()
        require('which-key').show({ global = false })
      end,
      desc = 'Buffer Local Keymaps (which-key)',
    },
    {
      '<C-w><space>',
      function()
        require('which-key').show({ keys = '<C-w>', loop = true })
      end,
    },
  },
}
