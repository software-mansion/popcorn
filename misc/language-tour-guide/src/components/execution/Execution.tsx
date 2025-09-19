import { History } from "./History";
import { Results } from "./Results";

export default function Execution() {
  return (
    <section className="bg-light-30 border-grey-20 scrollbar relative flex min-h-100 flex-col gap-4 overflow-y-scroll rounded-md border pb-20 lg:min-h-60 lg:pb-4">
      <Results />
      <History />
    </section>
  );
}
