<?php if (!defined('ABSPATH')) { exit; } ?>
<section class="hero" aria-labelledby="hero-title">
    <img class="hero-image" src="<?php echo esc_url(flynns_arcade_neon_asset('hero-arcade-kid.png')); ?>" alt="A boy with a backpack in a neon arcade overlooking the city" fetchpriority="high" width="742" height="238">
    <div class="section-inner hero-inner"><div class="hero-copy">
        <p class="eyebrow">Flynn's Arcade Publishing</p>
        <h1 id="hero-title"><span><?php echo esc_html($site['hero_title'][0]); ?></span><span><?php echo esc_html($site['hero_title'][1]); ?></span></h1>
        <p class="hero-description"><?php echo esc_html($site['hero_copy']); ?></p>
        <div class="hero-platform"><?php flynns_arcade_neon_platform_icon('Switch'); ?><span>Nintendo Switch</span></div>
        <a class="arcade-button" href="<?php echo esc_url(flynns_arcade_neon_url('game', flynns_arcade_neon_slug($site['featured_game']))); ?>">Discover the game <?php flynns_arcade_neon_icon('arrow-right'); ?></a>
    </div></div>
</section>
<?php flynns_arcade_neon_promotions(); ?>
<?php require get_template_directory() . '/templates/catalog.php'; ?>
