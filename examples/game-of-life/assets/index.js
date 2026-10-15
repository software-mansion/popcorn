import { Popcorn } from "@swmansion/popcorn";

const popcorn = await Popcorn.init({
  onStdout: console.log,
});

await popcorn.call("game_of_life_ui", "mount");
