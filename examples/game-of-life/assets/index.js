import { Popcorn } from "@swmansion/popcorn";

const result = await Popcorn.tryInit({
  onStdout: console.log,
});

if (!result.ok) throw result.error;

const mounted = await result.data.tryCall("game_of_life_ui", "mount");
if (!mounted.ok) throw mounted.error;
