import { Popcorn } from "@swmansion/popcorn";

const result = await Popcorn.init({
  beam: {
    otpAssetsRoot: "/assets/otp/",
  },
  onStdout: console.log,
});

if (!result.ok) {
  console.error("Popcorn failed to boot:", result.error);
}
