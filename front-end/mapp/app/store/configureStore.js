import { createStore, applyMiddleware } from "redux";
import app from "../reducers/dataReducer";
import thunk from "redux-thunk";
import promiseMiddleware from "redux-promise-middleware";
import { composeWithDevTools } from "redux-devtools-extension";

export default function configureStore() {
  let store = createStore(
    app,
    composeWithDevTools(applyMiddleware(thunk, promiseMiddleware()))
  );
  return store;
}
