import { usePopcorn } from "../../utils/hooks/usePopcorn";
import { History } from "./History";
import { PopcornLoader } from "./PopcornLoader";
import { Results } from "./Results";

export default function Execution() {
  const { isLoadingPopcorn } = usePopcorn();

  return (
    <section className="bg-light-30 border-grey-20 scrollbar relative flex h-full min-h-100 flex-col gap-4 overflow-y-auto rounded-md border pb-28 lg:min-h-60 lg:pb-4">
      {isLoadingPopcorn ? (
        <PopcornLoader />
      ) : (
        <>
          <Results />
          <History />
        </>
      )}
    </section>
  );
}
