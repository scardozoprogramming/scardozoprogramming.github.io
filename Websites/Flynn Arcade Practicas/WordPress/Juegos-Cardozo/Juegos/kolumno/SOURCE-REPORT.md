# Kolumno — supplemental source report

Retrieved: 2026-11-06T07:04:05.247Z

## Sources and discovery

- Primary official page: https://www.flynnsarcades.com/kolumno/ (HTTP 200; saved verbatim as text in Fuentes/original.html).
- Exact page link discovered through the public WordPress search index: https://www.flynnsarcades.com/wp-json/wp/v2/search?search=Kolumno&per_page=100
- The homepage exposes the public API root through its `rel="https://api.w.org/"` link. The API index exposes `/wp/v2/search`. No candidate game URLs were guessed.
- Discovery response: Fuentes/discovery-search.json; selected official page record (ID 4473): Fuentes/page-discovery.json.
- Required field layout follows the local explosive-dinosaurs/JSON/game.json reference.

## Content mapping and checks

- JSON/game.json contains the title, destination slug, source, tagline, platforms, hero_video, gallery, poster, animation_url, stores, genres, ordered blocks and both source testimonials.
- Platforms/stores reproduce only the page's PURCHASE section: Nintendo Switch. Genre labels preserve the source's wording and order.
- Original main-gallery lightbox hrefs are used, in order, rather than thumbnails or invented optimized upload names. Image alt text is a plain descriptive label based on the title, not a quotation from the source's mostly empty alt attributes.
- Paragraphs separated by double line breaks become paragraph blocks; the source's feature heading and lists retain their order. HTML entities/whitespace are normalized and decorative outer testimonial quotes are removed; factual wording is retained. Header/footer, newsletter and OTHER GAMES are excluded.
- All 5 gallery originals, the poster and selected GIF returned HTTP 200 with their image content types. YouTube public oEmbed also returned HTTP 200 for the exact source trailer (Rbdx1j5yTWE), attributed to Flynn's Arcade. Detailed dated results are in Fuentes/media-checks.json. A successful oEmbed response establishes public metadata availability, not a full playback test.

## Source differences and gaps

- Exact title from the official index is “Kolumno”; visible headings use “KOLUMNO”.
- The trailer is present in the video widget settings, even though a plain-text extraction has no “Reproducir” label.
- Selected the original linked `kolumno_2.gif`. The poster is the original full-size `2021/08/kolumno.jpg`, rather than the generated Elementor thumbnail of Kolumno_cover_01.
- No required media field is missing from the official page. Store/review destination availability and individual external review text have not been independently verified; URLs/quotes are transcribed from the publisher's page. No additional platform availability or translations were supplied.

Only supplemental local source data was created in this game folder. WordPress, generators, templates and adjacent agents' files were not modified.
