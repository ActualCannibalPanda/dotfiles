return {
  'folke/which-key.nvim',
  event = 'VeryLazy',
  config = function()
    local wk = require('which-key')
    wk.add({
      { '<leader>h', group = 'Harpoon' },
    })
  end,
  keys = {
    {
      '<leader>?',
      function()
        require('which-key').show({ global = false })
      end,
      desc = 'Buffer Local Keymaps (which-key)',
    },
  },
}
