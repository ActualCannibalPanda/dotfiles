local s = vim.keymap.set
s('n', '<leader>lz', '<cmd>Lazy<cr>', { desc = 'Open Lazy UI' })
s('n', '<leader>x', '<cmd>nohlsearch<cr>', { desc = 'Stop Highlighting search' })
s('n', '<C-t>c', '<cmd>tabc<cr>', { desc = 'Close tab' })
