# SmokeSignal

[SmokeSignal](https://smokesignal.eth.link/) uses the Ethereum blockchain to facilitate uncensorable, global chat.

### Requirements
- [`npm`](https://docs.npmjs.com/cli/v7/commands/npm)

### Setup
- `git submodule update --init --recursive`
- `npm install`

### Build instructions
- `npm run build`
- Assets can be found in `./public`

### Development instructions
- `npm run dev`
- Navigate to `http://localhost:8000/`

### Environment Variables
| Key | Description |
|-|-|
| `ENV` | Set to `production` or `development`. |
| `ETH_PROVIDER_URL` | The provider URL for querying the Ethereum network. |
| `XDAI_PROVIDER_URL` | The provider URL for querying the xDai network. |
| `FAUCET_TOKEN` | The secret used to authenticate xDai faucet requests. |
| `GA_TRACKING_ID` | The Google Analytics tracking ID. |
| `FB_TRACKING_ID` | The Facebook Analytics tracking ID. |