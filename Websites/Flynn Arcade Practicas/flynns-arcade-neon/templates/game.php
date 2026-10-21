<?php
if (!defined('ABSPATH')) { exit; }
$game = flynns_arcade_neon_current_game();
$slug = flynns_arcade_neon_slug($game['title']);
$detail = flynns_arcade_neon_data('game-details')[$slug];
$platforms = array_unique(array_merge($game['platforms'], array_column($detail['stores'], 'platform')));
$feature_start = count($detail['blocks']);
foreach ($detail['blocks'] as $index => $block) {
    if ('paragraph' !== $block['type']) { $feature_start = $index; break; }
}
?>
<section class="game-showcase" aria-labelledby="game-title">
    <div class="section-inner game-page">
        <a class="text-link back-link" href="<?php echo esc_url(flynns_arcade_neon_url('games') . '#games'); ?>"><?php flynns_arcade_neon_icon('arrow-left'); ?>All games</a>
        <div class="game-showcase-layout">
            <div class="game-detail-copy">
                <p class="eyebrow">Flynn's Arcade Publishing</p>
                <h1 id="game-title"><?php echo esc_html($game['title']); ?></h1>
                <p class="game-tagline"><?php echo esc_html($detail['tagline']); ?></p>
                <div class="game-platform-list" aria-label="Platforms"><?php foreach ($platforms as $platform) : ?><span><?php flynns_arcade_neon_platform_icon($platform); ?><span><?php echo esc_html('Switch' === $platform ? 'Nintendo Switch' : $platform); ?></span></span><?php endforeach; ?></div>
                <div class="game-actions"><a class="arcade-button" href="#purchase"><?php flynns_arcade_neon_icon('shopping-bag'); ?>Get the game</a><?php if ($detail['trailers']) : ?><a class="text-link" href="#trailer"><?php flynns_arcade_neon_icon('play'); ?>Watch trailer</a><?php endif; ?></div>
            </div>
            <?php flynns_arcade_neon_game_video($detail['hero_video'], $detail['gallery'][0], $game['title'], 'game-video'); ?>
        </div>
    </div>
</section>
<?php if ($detail['gallery']) : ?>
<section class="game-gallery" aria-label="<?php echo esc_attr($game['title'] . ' screenshot gallery'); ?>" data-gallery>
    <?php foreach ($detail['gallery'] as $image) : ?>
        <a href="<?php echo esc_url(flynns_arcade_neon_asset($image['image'])); ?>" data-gallery-item title="View screenshot"><img src="<?php echo esc_url(flynns_arcade_neon_asset($image['image'])); ?>" alt="<?php echo esc_attr($image['alt']); ?>" width="<?php echo (int) $image['width']; ?>" height="<?php echo (int) $image['height']; ?>" loading="lazy" decoding="async"><span class="game-gallery-zoom" aria-hidden="true"><?php flynns_arcade_neon_icon('maximize-2'); ?></span></a>
    <?php endforeach; ?>
</section>
<?php endif; ?>
<section class="game-purchase" id="purchase" aria-labelledby="purchase-title"><div class="section-inner game-purchase-inner">
    <h2 class="section-title" id="purchase-title">Purchase</h2>
    <div class="game-store-links"><?php foreach ($detail['stores'] as $store) : ?><a class="arcade-button" href="<?php echo esc_url($store['url']); ?>" target="_blank" rel="noopener noreferrer" aria-label="<?php echo esc_attr($game['title'] . ' on ' . $store['label']); ?>"><?php flynns_arcade_neon_platform_icon($store['platform']); ?><?php echo esc_html($store['label']); ?><?php flynns_arcade_neon_icon('external-link'); ?></a><?php endforeach; ?></div>
</div></section>
<section class="game-content-band" aria-labelledby="description-title"><div class="section-inner game-overview-layout">
    <aside class="game-artwork"><img class="game-detail-image" src="<?php echo esc_url(flynns_arcade_neon_asset($detail['poster']['image'])); ?>" alt="<?php echo esc_attr($detail['poster']['alt']); ?>" width="<?php echo (int) $detail['poster']['width']; ?>" height="<?php echo (int) $detail['poster']['height']; ?>" loading="lazy" decoding="async"></aside>
    <div class="game-story">
        <h2 class="screen-reader-text" id="description-title"><?php echo esc_html('About ' . $game['title']); ?></h2>
        <div class="game-genres"><?php foreach ($detail['genres'] as $genre) : ?><span><?php echo esc_html($genre); ?></span><?php endforeach; ?></div>
        <div class="game-description"><?php flynns_arcade_neon_game_blocks(array_slice($detail['blocks'], 0, $feature_start)); ?></div>
        <div class="game-features-media<?php echo $detail['trailers'] && $feature_start < count($detail['blocks']) ? ' has-features' : ''; ?>">
            <?php if ($feature_start < count($detail['blocks'])) : ?><div class="game-features"><?php flynns_arcade_neon_game_blocks(array_slice($detail['blocks'], $feature_start)); ?></div><?php endif; ?>
            <?php if ($detail['trailers']) flynns_arcade_neon_game_video($detail['trailers'][0], $detail['gallery'][0], $game['title']); ?>
        </div>
        <?php if ($detail['reviews']) : $review_slides = array_chunk($detail['reviews'], 2); ?>
        <section class="game-reviews" data-carousel aria-label="Press reviews" aria-roledescription="carousel" tabindex="0">
            <div class="game-reviews-heading"><h2>Press reviews</h2><?php flynns_arcade_neon_carousel_controls(count($review_slides), 'review'); ?></div>
            <div class="game-review-slides"><?php foreach ($review_slides as $index => $reviews) : ?>
                <div class="game-review-slide" data-carousel-slide<?php echo $index ? ' hidden' : ''; ?>><?php foreach ($reviews as $review) : ?>
                    <figure class="game-review"><blockquote><?php echo esc_html($review['quote']); ?></blockquote><figcaption><span><?php echo esc_html($review['author']); ?></span><?php if ($review['url']) : ?><a href="<?php echo esc_url($review['url']); ?>" target="_blank" rel="noopener noreferrer"><?php echo esc_html($review['outlet']); ?><?php flynns_arcade_neon_icon('external-link'); ?></a><?php else : ?><span><?php echo esc_html($review['outlet']); ?></span><?php endif; ?></figcaption></figure>
                <?php endforeach; ?></div>
            <?php endforeach; ?></div>
        </section>
        <?php endif; ?>
    </div>
</div></section>
<section class="section-inner other-games"><div class="section-heading"><h2 class="section-title">Other Games</h2></div><div class="games-grid related-games-grid related-games-grid-<?php echo count($detail['related']); ?>"><?php foreach ($detail['related'] as $related_slug) : foreach ($games as $other) : if (flynns_arcade_neon_slug($other['title']) !== $related_slug) continue; flynns_arcade_neon_game_card($other); break; endforeach; endforeach; ?></div></section>
<?php flynns_arcade_neon_gallery_dialog($game['title'] . ' screenshots'); ?>
