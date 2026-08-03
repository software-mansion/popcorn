import { Popcorn } from "@swmansion/popcorn";

const result = await Popcorn.init({
  beam: {
    manifestUrl: "/assets/otp/manifest.json",
  },
  onStdout: console.log,
});

if (!result.ok) throw result.error;

const mounted = await result.data.genserver.call("game_of_life_ui", "mount");
if (!mounted.ok) throw mounted.error;
