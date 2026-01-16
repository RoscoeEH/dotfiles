// Config docs:
//
//   https://glide-browser.app/config
//
// API reference:
//
//   https://glide-browser.app/api
//
// Default config files can be found here:
//
//   https://github.com/glide-browser/glide/tree/main/src/glide/browser/base/content/plugins
//
// Most default keymappings are defined here:
//
//   https://github.com/glide-browser/glide/blob/main/src/glide/browser/base/content/plugins/keymaps.mts
//
// Try typing `glide.` and see what you can do!


glide.o.hint_size = "16px";

// Security settings
glide.prefs.set("privacy.trackingprotection.enabled", true);
glide.prefs.set("privacy.bounceTrackingProtection.mode", 1);

// Copy link from hint
glide.keymaps.set("normal", "ys", () =>
  glide.hints.show({
    selector: "[href]",
    async action({ content }) {
      const href = await content.execute((target) => (target as HTMLAnchorElement).href);
      await navigator.clipboard.writeText(href);
    },
  }));