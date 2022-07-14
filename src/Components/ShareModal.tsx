import { useOnEscOrClickOutside } from "../hooks/useOnEscOrClickOutside";
import "./ShareModal.css";
import { FileContent } from "./filecontent";
import {
  appUrlPrefix,
  editorUrlPrefix,
  fileContentsToUrlString,
} from "./share";
import * as React from "react";

// =============================================================================
// ShareModal component
// =============================================================================

export function ShareModal({
  fileContents = [],
  setShowShareModal,
}: {
  fileContents: FileContent[];
  setShowShareModal: React.Dispatch<React.SetStateAction<boolean>>;
}) {
  const showModalRef = React.useRef<HTMLDivElement>(null);

  const encodedCode = fileContentsToUrlString(fileContents);

  const editorUrl = editorUrlPrefix + encodedCode;
  const appUrl = appUrlPrefix + encodedCode;

  const editorUrlInputRef = React.useRef<HTMLInputElement>(null);
  const appUrlInputRef = React.useRef<HTMLInputElement>(null);

  const [editorButtonText, setEditorButtonText] = React.useState("Copy URL");
  const [appButtonText, setAppButtonText] = React.useState("Copy URL");

  useOnEscOrClickOutside(showModalRef, () => setShowShareModal(false));

  return (
    <div className="ShareModal" ref={showModalRef}>
      <div>
        <label>Editor URL ({editorUrl.length} bytes)</label>
        <div className="ShareModal--row">
          <span className="ShareModal--url">
            <input
              value={editorUrl}
              ref={editorUrlInputRef}
              className="ShareModal--urlinput"
              onFocus={(e) => e.target.select()}
              readOnly
            ></input>
          </span>
          <button
            style={{ whiteSpace: "nowrap", width: "8em" }}
            onClick={() => {
              if (!editorUrlInputRef.current) return;
              editorUrlInputRef.current.select();
              navigator.clipboard.writeText(editorUrlInputRef.current.value);

              setEditorButtonText("\u2713");
              setTimeout(() => setEditorButtonText("Copy URL"), 800);
            }}
          >
            {editorButtonText}
          </button>
        </div>
      </div>
      <div>
        <label>Application URL ({appUrl.length} bytes)</label>
        <div className="ShareModal--row">
          <span className="ShareModal--url">
            <input
              value={appUrl}
              ref={appUrlInputRef}
              className="ShareModal--urlinput"
              onFocus={(e) => e.target.select()}
              readOnly
            ></input>
          </span>
          <button
            style={{ whiteSpace: "nowrap", width: "8em" }}
            onClick={() => {
              if (!appUrlInputRef.current) return;
              appUrlInputRef.current.select();
              navigator.clipboard.writeText(appUrlInputRef.current.value);

              setAppButtonText("\u2713");
              setTimeout(() => setAppButtonText("Copy URL"), 800);
            }}
          >
            {appButtonText}
          </button>
        </div>
      </div>
    </div>
  );
}
