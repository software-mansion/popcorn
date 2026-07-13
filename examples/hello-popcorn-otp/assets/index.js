import { Popcorn } from "@swmansion/popcorn-otp";

const result = await Popcorn.init({
  beam: {
    manifestUrl: "/assets/otp/manifest.json",
  },
  onStdout: console.log,
});

if (!result.ok) {
  console.error("Popcorn failed to boot:", result.error);
}
