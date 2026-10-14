<?php
/** Compact reference-layout homepage. @package FlynnsArcadeNeon */
if (!defined('ABSPATH')) { exit; }
$games = flynns_arcade_neon_games();
$editorial = flynns_arcade_neon_editorial();
get_header();
?>
<main id="main-content">
    <section class="hero" aria-labelledby="hero-title">
        <div class="hero-art" role="img" aria-label="Neon arcade overlooking a pixel-art city at sunset"><?php flynns_arcade_neon_art('660 70 1012 299'); ?></div>
        <div class="section-inner hero-inner">
            <div class="hero-copy">
                <h1 class="hero-title" id="hero-title"><span class="cyan">Indie Games.</span><span class="pink">Arcade Soul.</span></h1>
                <p><?php foreach ($editorial['hero_copy'] as $index => $line) : ?><?php echo $index ? '<br>' : ''; ?><?php echo esc_html($line); ?><?php endforeach; ?></p>
                <a class="arcade-button" href="#games">Explore Games <?php flynns_arcade_neon_icon('arrow-right'); ?></a>
            </div>
        </div>
    </section>

    <section class="games-section" id="games" aria-labelledby="games-title">
        <div class="section-inner">
            <div class="section-heading">
                <h2 class="section-title" id="games-title">Latest Games</h2>
                <a class="view-link" href="#catalog" data-dialog="catalog">View All Games <?php flynns_arcade_neon_icon('arrow-right'); ?></a>
            </div>
            <div class="games-grid">
                <?php foreach (array_slice($games, 0, 6) as $game) : flynns_arcade_neon_game_card($game); endforeach; ?>
            </div>
        </div>
    </section>

    <section class="spotlight-section" id="indies" aria-label="Indie spotlight and newsletter">
        <div class="section-inner spotlight-grid">
            <article class="indie-panel" aria-labelledby="indies-title">
                <div class="indie-image" role="img" aria-label="A small adventurer looking out over a forest and distant towers"><?php flynns_arcade_neon_art('132 683 353 164'); ?></div>
                <div class="indie-content">
                    <p class="section-kicker">Indie Spotlight</p>
                    <h2 id="indies-title" data-spotlight-title><?php echo esc_html($editorial['spotlights'][0]['title']); ?></h2>
                    <p data-spotlight-copy><?php echo esc_html($editorial['spotlights'][0]['summary']); ?></p>
                    <a class="arcade-button" href="#studios" data-dialog="studios">Discover Indies <?php flynns_arcade_neon_icon('arrow-right'); ?></a>
                </div>
                <div class="spotlight-controls" aria-label="Indie spotlight slides">
                    <button class="icon-button" type="button" data-spotlight-step="-1" aria-label="Previous spotlight" title="Previous spotlight"><?php flynns_arcade_neon_icon('chevron-left'); ?></button>
                    <div class="spotlight-dots">
                        <?php foreach ($editorial['spotlights'] as $i => $spotlight) : ?>
                            <button class="spotlight-dot" type="button" data-spotlight-index="<?php echo $i; ?>" aria-label="Spotlight <?php echo $i + 1; ?>" aria-pressed="<?php echo 0 === $i ? 'true' : 'false'; ?>"></button>
                        <?php endforeach; ?>
                    </div>
                    <button class="icon-button" type="button" data-spotlight-step="1" aria-label="Next spotlight" title="Next spotlight"><?php flynns_arcade_neon_icon('chevron-right'); ?></button>
                </div>
            </article>
            <aside class="newsletter-panel" aria-labelledby="newsletter-title">
                <h2 id="newsletter-title">Stay In The Game</h2>
                <p><?php echo esc_html($editorial['newsletter_copy']); ?></p>
                <form class="newsletter-form" data-newsletter>
                    <label class="screen-reader-text" for="newsletter-email">Your email address</label>
                    <input id="newsletter-email" type="email" name="email" placeholder="Your email address" autocomplete="email" required>
                    <button type="submit">Subscribe</button>
                </form>
                <p class="form-note" data-newsletter-note role="status" hidden></p>
                <div class="social-row" aria-label="Social channels">
                    <span class="social-mark reference-art social-twitter" role="img" aria-label="X" title="X"></span>
                    <span class="social-mark reference-art social-instagram" role="img" aria-label="Instagram" title="Instagram"></span>
                    <span class="social-mark reference-art social-youtube" role="img" aria-label="YouTube" title="YouTube"></span>
                    <span class="social-mark" title="Games"><?php flynns_arcade_neon_icon('gamepad2'); ?><span class="screen-reader-text">Games</span></span>
                    <a class="social-mark" href="#press" data-dialog="press" aria-label="Press information" title="Press information"><?php flynns_arcade_neon_icon('mail'); ?></a>
                </div>
            </aside>
        </div>
    </section>
</main>

<dialog class="content-dialog" id="catalog" aria-labelledby="catalog-title">
    <?php flynns_arcade_neon_dialog_header('catalog', 'The Games'); ?>
    <div class="catalog-toolbar">
        <div class="filter-group" aria-label="Filter games">
            <?php foreach (flynns_arcade_neon_filters() as $key => $label) : ?>
                <button class="filter-button" type="button" data-filter="<?php echo esc_attr($key); ?>" aria-pressed="<?php echo 'all' === $key ? 'true' : 'false'; ?>"><?php echo esc_html__($label, 'flynns-arcade-neon'); ?></button>
            <?php endforeach; ?>
        </div>
        <label class="search-wrap"><?php flynns_arcade_neon_icon('search'); ?><span class="screen-reader-text">Search games</span><input type="search" data-game-search placeholder="Search games"></label>
    </div>
    <div class="games-grid catalog-grid">
        <?php foreach ($games as $game) : flynns_arcade_neon_game_card($game); endforeach; ?>
    </div>
    <p class="empty-state" data-empty-state hidden>No games match your search.</p>
    <div class="game-details" data-game-details hidden tabindex="-1" aria-live="polite"><h3 data-detail-title></h3><p data-detail-summary></p><p data-detail-platforms></p></div>
</dialog>

<dialog class="content-dialog" id="studios" aria-labelledby="studios-title">
    <?php flynns_arcade_neon_dialog_header('studios', 'Meet The Indies'); ?>
    <p class="dialog-copy">Meet the independent creators behind our games.</p>
    <div class="studio-list">
        <?php foreach ($editorial['studios'] as $studio) : ?>
            <article><h3><?php echo esc_html($studio['name']); ?></h3><p><?php echo esc_html($studio['detail']); ?></p></article>
        <?php endforeach; ?>
    </div>
</dialog>

<dialog class="content-dialog" id="about" aria-labelledby="about-title">
    <?php flynns_arcade_neon_dialog_header('about', "About Flynn's Arcade"); ?>
    <div class="dialog-copy">
        <?php foreach ($editorial['about'] as $section) : ?>
            <h3><?php echo esc_html($section['title']); ?></h3>
            <?php foreach ($section['paragraphs'] as $paragraph) : ?><p><?php echo esc_html($paragraph); ?></p><?php endforeach; ?>
        <?php endforeach; ?>
    </div>
</dialog>

<dialog class="content-dialog" id="press" aria-labelledby="press-title">
    <?php flynns_arcade_neon_dialog_header('press', 'Press Media'); ?>
    <div class="dialog-copy">
        <p>Company news highlights and coverage from the media.</p>
        <h3>Press contact</h3>
        <p class="press-email"><?php echo esc_html($editorial['contact_email']); ?></p>
    </div>
    <div class="press-list">
        <?php foreach ($editorial['press'] as $item) : ?>
            <article class="press-mention"><div><h3><?php echo esc_html($item['title']); ?></h3><p><?php echo esc_html($item['outlet']); ?><?php echo $item['platform'] ? ' / ' . esc_html($item['platform']) : ''; ?></p></div><span><?php echo esc_html($item['date']); ?></span></article>
        <?php endforeach; ?>
    </div>
</dialog>
<script id="spotlight-data" type="application/json"><?php echo wp_json_encode($editorial['spotlights'], JSON_HEX_TAG | JSON_HEX_AMP | JSON_HEX_APOS | JSON_HEX_QUOT); ?></script>
<noscript><p class="no-script">Enable JavaScript to explore the full catalog and company information.</p></noscript>
<?php get_footer(); ?>
