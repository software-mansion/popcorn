import { Popcorn } from '@swmansion/popcorn';
import { LiveSocketInstanceInterface } from 'phoenix_live_view';

interface LLVConfig {
    /** Paths to compiled WASM bundle files. Defaults to `["wasm/bundle.avm"]` */
    bundlePaths?: string[];
    /** Enable Popcorn debug logging */
    debug?: boolean;
    /** Callback for raw Popcorn messages */
    eventHandler?: (msg: unknown) => void;
    /**
     * Override LLV's default navigation handler.
     * Called instead of `liveSocket.historyPatch` when an LLV view calls `push_patch`.
     * Pass a custom function to take full control of navigation.
     */
    onNavigate?: (href: string, replace: boolean) => void;
}
/** Opaque rendered diff payload delivered from WASM via structuredClone */
type RenderedDiff = Record<string, unknown>;
declare global {
    interface Window {
        __popcornTransportReceive: (llvId: string, diff: RenderedDiff) => void;
        __llvSync?: (id: string, eventName: string, payload: Record<string, unknown>) => void;
        __llvPushServer?: (llvId: string, event: string, payload: Record<string, unknown>) => void;
    }
}

type CallResult = Awaited<ReturnType<Popcorn["call"]>>;
interface CreateArgs {
    id: string;
    view: string | null;
    url: string;
    urlParams: Record<string, string>;
    assigns: Record<string, unknown>;
}
declare class PopcornClient {
    private popcorn;
    get ready(): boolean;
    attach(popcorn: Popcorn): void;
    private call;
    private fire;
    create({ id, view, url, urlParams, assigns }: CreateArgs): Promise<CallResult>;
    destroy(id: string): void;
    reconnected(id: string): void;
    updateAssigns(id: string, assigns: Record<string, unknown>): void;
    handleParams(id: string, params: Record<string, string>, url: string): void;
    handleTransportFrame(id: string, event: string, payload: unknown): Promise<CallResult>;
    serverMessage(id: string, payload: Record<string, unknown>): void;
    pushError(id: string, event: string, payload: Record<string, unknown>): void;
    push(id: string, event: string, payload: Record<string, unknown>): Promise<CallResult>;
}
declare class LLVEngine {
    private socket;
    private config;
    private pop;
    private views;
    private channels;
    private bufferedServerMessages;
    private eventBusHooks;
    private popcornLink;
    private constructor();
    /**
     * Initializes LLVEngine and connects the LiveSocket.
     *
     * @param liveSocket - The phoenix_live_view LiveSocket instance.
     * @param config - Optional LLV configuration.
     */
    static create(liveSocket: LiveSocketInstanceInterface, config?: LLVConfig): Promise<LLVEngine>;
    private socketClass;
    private connectPopcornSocket;
    private mountView;
    private unmountView;
    private registerServerMessageListener;
    private registerHooks;
    private bindFormsIfHostless;
    private bootPopcorn;
    private setupMirrorChannels;
    private exposeGlobals;
    private patchOwner;
    private scanAndMount;
    private flushBufferedServerMessages;
    /**
     * Pushes an event into a LLVEngine from external JavaScript.
     *
     * @param viewId - The view name (e.g. `"ThermostatLive"`) or element id.
     * @param event - The event name to dispatch into the view's `handle_info/2`.
     * @param payload - Optional payload map passed alongside the event.
     */
    pushEvent(viewId: string, event: string, payload?: Record<string, unknown>): Promise<void>;
}

export { LLVEngine, PopcornClient };
export type { LLVConfig };
