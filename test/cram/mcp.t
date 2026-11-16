The MCP server answers the protocol handshake and tool listing without
touching the network.

  $ cat > requests <<'REQ'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"cram","version":"0"}}}
  > {"jsonrpc":"2.0","method":"notifications/initialized"}
  > {"jsonrpc":"2.0","id":2,"method":"ping"}
  > {"jsonrpc":"2.0","id":3,"method":"tools/list"}
  > {"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"deploy_status","arguments":{"target":"not a target"}}}
  > {"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"nope","arguments":{}}}
  > {"jsonrpc":"2.0","id":6,"method":"bogus"}
  > not json
  > REQ
  $ GITHUB_TOKEN=unused oeil mcp < requests > replies
  $ jq -c 'if .result.tools then {id, tools: [.result.tools[].name]} elif .result.serverInfo then {id, version: .result.protocolVersion, server: .result.serverInfo.name} else . end' replies | sort
  {"id":1,"version":"2025-06-18","server":"oeil"}
  {"id":3,"tools":["deploy_status","wait_for_deploy","pipeline_status","repo_status","pr_timeline","job_log"]}
  {"jsonrpc":"2.0","id":2,"result":{}}
  {"jsonrpc":"2.0","id":4,"result":{"content":[{"type":"text","text":"cannot parse target \"not a target\": expected owner/repo#PR, owner/repo@SHA or owner/repo:branch"}],"isError":true}}
  {"jsonrpc":"2.0","id":5,"result":{"content":[{"type":"text","text":"unknown tool: nope"}],"isError":true}}
  {"jsonrpc":"2.0","id":6,"error":{"code":-32601,"message":"method not found: bogus"}}
  {"jsonrpc":"2.0","id":null,"error":{"code":-32700,"message":"parse error: Line 1, bytes 0-8:\nInvalid token 'not json'"}}

An unknown protocol version gets the latest one we support.

  $ echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"1999-01-01"}}' | GITHUB_TOKEN=unused oeil mcp | jq -r .result.protocolVersion
  2025-06-18
