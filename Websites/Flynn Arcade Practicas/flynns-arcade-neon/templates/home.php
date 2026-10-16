<?php
if (!defined('ABSPATH')) { exit; }
$featured = null;
foreach ($games as $game) {
    if ($game['title'] === $site['featured_game']) { $featured = $game; break; }
}
?>
<section class="hero" aria-label="Flynn's Arcade">
    <img class="hero-image" src="<?php echo esc_url(flynns_arcade_neon_asset('hero-arcade-banner.webp')); ?>" alt="Panoramic neon arcade with a boy overlooking the city, surrounded by arcade machines" fetchpriority="high" width="2172" height="724">
</section>
<section class="featured-game" aria-labelledby="featured-title">
    <div class="section-inner featured-game-inner"><div class="featured-game-copy">
        <p class="eyebrow">Flynn's Arcade Publishing</p>
        <h1 id="featured-title"><span><?php echo esc_html($site['hero_title'][0]); ?></span><span><?php echo esc_html($site['hero_title'][1]); ?></span></h1>
        <p class="featured-game-description"><?php echo esc_html($site['hero_copy']); ?></p>
        <div class="featured-game-platform"><?php flynns_arcade_neon_platform_icon('Switch'); ?><span>Nintendo Switch</span></div>
        <a class="arcade-button" href="<?php echo esc_url(flynns_arcade_neon_url('game', flynns_arcade_neon_slug($site['featured_game']))); ?>">Discover the game <?php flynns_arcade_neon_icon('arrow-right'); ?></a>
    </div>
    <?php if ($featured) : ?><img class="featured-game-image" src="<?php echo esc_url(flynns_arcade_neon_asset($featured['image'])); ?>" alt="<?php echo esc_attr($featured['title']); ?>" width="640" height="360" decoding="async"><?php endif; ?>
    </div>
</section>
<?php flynns_arcade_neon_promotions(); ?>
<?php require get_template_directory() . '/templates/catalog.php'; ?>
