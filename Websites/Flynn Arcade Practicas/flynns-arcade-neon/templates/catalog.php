<?php if (!defined('ABSPATH')) { exit; } ?>
<section class="catalog section-inner" id="games" aria-labelledby="games-title" data-catalog>
    <h2 id="games-title" class="screen-reader-text">Games</h2>
    <div class="catalog-toolbar"><div class="filter-group" aria-label="Game categories"><?php foreach (flynns_arcade_neon_filters() as $key => $label) : ?><button class="filter-button" type="button" data-filter="<?php echo esc_attr($key); ?>" aria-pressed="<?php echo 'all' === $key ? 'true' : 'false'; ?>"><?php echo esc_html($label); ?></button><?php endforeach; ?></div><label class="search-wrap"><?php flynns_arcade_neon_icon('search'); ?><span class="screen-reader-text">Search games</span><input type="search" data-game-search placeholder="Search games"></label></div>
    <div class="games-grid"><?php foreach ($games as $game) : flynns_arcade_neon_game_card($game, true); endforeach; ?></div>
    <p class="empty-state" data-empty-state hidden>No games match your search.</p>
    <div class="load-more-row"><button class="arcade-button" data-load-more type="button" hidden>Load more <?php flynns_arcade_neon_icon('plus'); ?></button><p data-catalog-count aria-live="polite"></p></div>
</section>
