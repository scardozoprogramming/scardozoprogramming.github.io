# Point & Click Cardozo — native category exports

Prepared locally: 2026-11-06. Authorized destination: WordPress page **6984**.

## Classification and page contents

Point & Click is a genre catalogue, not a standalone game. The new exports contain a native English genre introduction with the H1 **Point & Click**, followed by the complete set of explicitly tagged point-click catalogue cards. The former designer placeholders (Amanda Lee / Adam Cheise) are excluded. No game data file was created for this category.

## Exact category selection

**1 card: Dexter Stardust.**

- Source native card: `WordPress/Games-Cardozo/Elementor/Games-Cardozo-bloques.json`, original model `fc6003e`. Also cross-checked the matching card in `WordPress/Inicio-Cardozo/Elementor/Inicio-Cardozo-bloques.json`.
- Actual classes: `fc-card fc-category-adventure fc-category-narrative fc-category-point-click fc-category-puzzle`.
- `Games/Juegos/dexter-stardust/JSON/game.json` and `WordPress/GAMES/Juegos/dexter-stardust/JSON/game.json` both list categories `adventure, narrative, point-click, puzzle`. All 32 local catalogue game records were checked; Dexter Stardust is the only explicit point-click match. Narrative/adventure membership alone was not treated as point-click membership.
- Existing catalogue destination retained: https://www.flynnsarcades.com/dexter/
- Existing cover retained: https://www.flynnsarcades.com/wp-content/uploads/2026/10/fc-home-dexter-stardust-cover.webp
- Existing animated preview retained: https://www.flynnsarcades.com/wp-content/uploads/2026/10/fc-home-dexter-stardust-hover.webp
- Existing preview fallback retained: https://www.flynnsarcades.com/wp-content/uploads/2021/12/Gif_Dexter.gif
- The complete native card, including image attachment ID, preview attributes, genre tags, Nintendo Switch/Xbox icons and existing store/navigation links, is copied unchanged apart from unique Elementor model/repeater IDs.

## Template reuse and editability

`WordPress/Plantilla-Juegos/Elementor/Game-plantilla.json` supplies embedded fonts, generic setup styles, the unchanged bounded preview-loader implementation, native hero typography, the responsive `fc-game-related` grid, shared header shortcode `[elementor-template id="7123"]`, and the final three common footer sections. The support section retains `.fc-game-setup`; hero rules are scoped through that class without page IDs. The category initializer only starts card previews and does not create game dialogs or game sections.

Native editable structure: setup support HTML widget; shared header reference; hero button, eyebrow, H1 and introduction; H2 catalogue title and native game card widgets; common footer widgets. The only HTML widget carries fonts/CSS/preview behavior, not flattened page content. Native section anchors are `overview` and `catalogue`. No trailer, category-level platforms/purchase block, gameplay/gallery, reviews, search or load-more controls are included. Platform icons within the authentic game card are retained.

## Deliverables and validation

- `Elementor/plantilla.json`: native Elementor 0.4 page export, matching the game-folder template schema.
- `Elementor/portapapeles.json`: matching `type/siteurl/elements` clipboard schema; elements equal the template content.
- JSON parsing, seven native top-level sections, unique 7-character model IDs and repeater IDs, one H1, header 7123, card equality against source (except IDs), and common footer preservation checked.
- Hero checked for absence of What Comes After title/media/tagline and designer placeholders. Category selection checked against all local game category records.
- Inline JavaScript syntax checked; preview initialization checked with a minimal local document stub, including duplicate-initialization prevention.
- Read-only source hashes checked unchanged. Writes are restricted to this point-click folder.

## Limitations and handoff

Local exports only: no browser control, live Elementor paste/render check, WordPress update, shared-template edit or publishing was performed. The parent can paste through the visible Elementor UI into page 6984. Header 7123, existing remote media and store URLs remain external dependencies; their live availability was not rechecked. The one-card catalogue reflects explicit local category metadata; add further games only after their category data supports inclusion.

## Earlier saved source evidence

The existing `Fuentes/` files are retained unchanged. The previous source audit retrieved the official games/page/home sources on 2026-11-06 and established that the similarly named original Point & Click page was unfinished designer placeholder content, not evidence of a game. The original catalogue's legacy adventure filter is broader than the explicit local point-click category used for these exports. The new local export intentionally replaces that placeholder concept with the authorized genre catalogue.
