return {
  {
    'williamboman/mason.nvim',
    dependencies = { { 'williamboman/mason-lspconfig.nvim', module = 'mason' } },
    config = function()
      require('mason').setup({
        ui = {
          icons = {
            package_installed = '',
            package_pending = '',
            package_uninstalled = '',
          },
        },
      })
      require('mason-lspconfig').setup({
        ensure_installed = {
          'clangd',
          'haxe_language_server',
          'lua_ls',
          'pylyzer',
          'ols',
          'rust_analyzer',
        },
        automatic_install = true,
      })
    end,
  },
}
