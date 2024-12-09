return {
	"nvim-treesitter/nvim-treesitter",
	dependencies = { "HiPhish/nvim-ts-rainbow2" },
	build = ":TSUpdate",
	config = function()
		require("nvim-treesitter.configs").setup({
			ensure_installed = { "lua", "rust", "c", "cpp", "toml" },
			auto_install = true,
			sync_install = false,
			highlight = {
				enable = true,
				additional_vim_regex_highlighting = false,
			},
			indent = { enable = true },
			rainbow = {
				enable = true,
				query = "rainbow-parens",
				strategy = require("ts-rainbow").strategy.global,
			},
		})
	end,
}
