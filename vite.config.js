import { defineConfig } from "vite"
import { VitePWA } from 'vite-plugin-pwa'
import elmPlugin from "vite-plugin-elm"

export default defineConfig(({ mode }) => {
  const isDev = mode === 'development'
  return {
    base: isDev
      ? '/'
      : '/gems-game/',
    plugins: [
      VitePWA({
        registerType: 'autoUpdate',
        includeAssets: ['favicon.ico'],
        manifest: {
          name: "Gems",
          short_name: "Gems",
          display: "standalone",
          description: "A classic mini-game",
          background_color: '#222222',
          icons: [{
            src: "img/gems_icon.png",
            sizes: "512x512",
            type: "image/png",
          }, {
            src: "img/gems_maskable.png",
            sizes: "640x640",
            type: "image/png",
            purpose: "maskable",
          }],
        }
      }),
      elmPlugin({
        //debug: false,
      }),
    ],
  }
})
