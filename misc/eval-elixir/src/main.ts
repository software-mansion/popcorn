import './style.css'
import { Popcorn } from "@swmansion/popcorn";

document.querySelector<HTMLDivElement>('#app')!.innerHTML = `
  <div>
    <h1>Popcorn eval demo</h1>
    <textarea id="codeInput" class="code">IO.puts("hello")</textarea>
    <button id="run">Run</button>
    <div id="output" class="code"></div>
    <div id="result" class="code"></div>
  </div>
`

const code_input = document.querySelector("#codeInput")! as HTMLTextAreaElement;
const run_button = document.querySelector("#run")! as HTMLButtonElement;
const output_container = document.querySelector("#output")!;
const result_container = document.querySelector("#result")!;

const onLog = (log: any) => {
  output_container.textContent = output_container.textContent + "\n" + log;
}
const popcorn = await Popcorn.init({ onStdout: onLog, onStderr: onLog });

run_button.addEventListener("click", async () => {
  run_button.textContent = "Running..."
  run_button.setAttribute("disabled", "disabled");
  output_container.textContent = "";
  result_container.textContent = "";
  const result = await popcorn.call(["eval_elixir", code_input.value], { timeoutMs: 10_000 });
  if (result.ok) {
    result_container.textContent = result.data;
  } else {
    result_container.textContent = String(result.error);
  }
  run_button.textContent = "Run";
  run_button.removeAttribute("disabled");
});

code_input.addEventListener('keydown', (event: KeyboardEvent) => {
  if (event.key === 'Enter' && (event.metaKey || event.ctrlKey)) {
    event.preventDefault();
    run_button.click();
  }
});