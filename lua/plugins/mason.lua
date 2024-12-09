return {
	{
		"neovim/nvim-lspconfig",
	},
	{
		"williamboman/mason.nvim",
		dependencies = { "williamboman/mason-lspconfig.nvim", module = "mason" },
		config = function()
			require("mason").setup({
				ui = {
					icons = {
						package_installed = "",
						package_pending = "",
						package_uninstalled = "",
					},
				},
			})
			require("mason-lspconfig").setup({
				ensure_installed = { "lua_ls", "rust_analyzer", "clangd" },
				automatic_install = true,
			})

			local capabilities = require("cmp_nvim_lsp").default_capabilities()
			local lspconfig = require("lspconfig")
			lspconfig.lua_ls.setup({
				capabilities = capabilities,
				on_init = function(client)
					if client.workspace_folders then
						local path = client.workspace_folders[1].name
						if vim.loop.fs_stat(path .. "/.luarc.json") or vim.loop.fs_stat(path .. "/.luarc.jsonc") then
							return
						end
					end

					client.config.settings.Lua = vim.tbl_deep_extend("force", client.config.settings.Lua, {
						runtime = {
							-- Tell the language server which version of Lua you're using
							-- (most likely LuaJIT in the case of Neovim)
							version = "LuaJIT",
						},
						-- Make the server aware of Neovim runtime files
						workspace = {
							checkThirdParty = false,
							library = {
								vim.env.VIMRUNTIME,
								-- Depending on the usage, you might want to add additional paths here.
								-- "${3rd}/luv/library"
								-- "${3rd}/busted/library",
							},
						},
					})
				end,
				settings = {
					Lua = {},
				},
			})
			lspconfig.clangd.setup({
				capabilities = capabilities,
			})
			lspconfig.rust_analyzer.setup({
				capabilities = capabilities,
			})
		end,
	},
}
