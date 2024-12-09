return {
  "nvim-telescope/telescope.nvim",
  tag = "0.1.8",
  dependencies = { { "nvim-lua/plenary.nvim" } },
  keys = {
    {
      "<leader>ff",
      "<cmd>Telescope git_files<cr>",
      desc = "Telescope git files",
    },
    {
      "<leader>fg",
      "<cmd>Telescope live_grep<cr>",
      desc = "Telescope live grep",
    },
    {
      "<leader>fb",
      "<cmd>Telescope buffers<cr>",
      desc = "Telescope buffers",
    },
    {
      "<leader>fh",
      "<cmd>Telesope help_tags<cr>",
      desc = "Telescope help tags",
    },
  },
}
