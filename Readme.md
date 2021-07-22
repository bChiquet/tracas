Tracas
======

Tracas helps you collect data about your webpage usage.

# Features:

* Collect data about user actions on a page.
* Tag specific pages and DOM elements to get better data.
* Set Cookies for multi-session data (see cookie domain restrictions [1])
* Tracas can be configured if the provided defaults don't fit your needs.

# Use it:

To start collecting data, you just need to add this script to your website:
```html
<script async
  type="text/javascript"
  id="tracas-include-script"                  <!-- lets tracas know where to find its config -->
  app-name="example-app"                      <!-- app name is returned in reports -->
  server="https://example.com/reports"        <!-- The url to which reports should be sent -->
  src="tracas.js"                             <!-- the place where you host tracas script -->
></script>
```

That's it! Tracas should start sending data to your server when the page is visited.

# Build it:

Using nix:
* `nix-shell -p purescript -p spago -p pscid`
[using nix with purescript](https://www.srid.ca/purescript-nix)

Inside nix shell:

* build: `spago build` or `pscid`
* bundle: `spago bundle-app --to="example/tracas.js"`


[1]:https://developer.mozilla.org/en-US/docs/Web/Security/Same-origin_policy#cross-origin_data_storage_access
