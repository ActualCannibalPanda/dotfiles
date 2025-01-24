return {
  'folke/which-key.nvim',
  config = function()
    local wk = require('which-key')
    wk.add({
      mode = { 'n', 'v' },
      {
        '<leader>h',
        group = 'Harpoon',
      },
    })
    wk.setup()
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
