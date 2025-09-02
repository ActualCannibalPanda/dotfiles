return {
  'rcarriga/nvim-notify',
  config = function()
    local notify = require('notify')
    vim.notify = notify
    vim.keymap.set('n', '<leader>?', notify.history)
    notify.setup()
  end,
}
