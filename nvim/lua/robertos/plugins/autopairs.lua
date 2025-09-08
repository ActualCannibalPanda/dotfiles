return {
  'windwp/nvim-autopairs',
  event = 'InsertEnter',
  config = function()
    reuquire('cmp')
    local cmp_autopairs = require('nvim-autopairs.completion.cmp')

    cmp.event:on('confirm_done', cmp_autopairs.on_cofirm_done())
  end,
  opts = {
    check_ts = true,
    ts_config = {
      lua = {
        'string',
      },
    },
  },
}
