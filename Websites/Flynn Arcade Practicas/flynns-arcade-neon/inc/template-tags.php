<?php
/** Accessible shared components. @package FlynnsArcadeNeon */
if (!defined('ABSPATH')) { exit; }

function flynns_arcade_neon_icon(string $name): void {
    ?><svg class="icon" aria-hidden="true" focusable="false"><use href="#icon-<?php echo esc_attr($name); ?>"></use></svg><?php
}

function flynns_arcade_neon_brand(): void {
    ?><a class="brand" href="<?php echo esc_url(flynns_arcade_neon_url('home')); ?>" aria-label="Flynn's Arcade home"><img src="<?php echo esc_url(flynns_arcade_neon_asset(flynns_arcade_neon_data('structure')['logo'])); ?>" width="170" height="96" alt="Flynn's Arcade"></a><?php
}

function flynns_arcade_neon_nav(): void {
    $active = flynns_arcade_neon_view();
    ?>
    <nav class="site-nav" id="site-navigation" aria-label="Primary">
        <?php foreach (array('home' => 'Inicio', 'games' => 'Games', 'indies' => 'Indies', 'about' => 'About', 'press' => 'Press') as $view => $label) : ?>
            <a href="<?php echo esc_url(flynns_arcade_neon_url($view)); ?>"<?php echo ($active === $view || ('game' === $active && 'games' === $view)) ? ' aria-current="page"' : ''; ?>><?php echo esc_html($label); ?></a>
        <?php endforeach; ?>
    </nav>
    <?php
}

function flynns_arcade_neon_socials(bool $compact = false): void {
    $socials = flynns_arcade_neon_data('site')['socials'];
    if ($compact) $socials = array_slice($socials, 0, 5);
    ?><div class="social-links<?php echo $compact ? ' social-links-compact' : ''; ?>" aria-label="Social channels"><?php
    foreach ($socials as $social) :
        ?><a href="<?php echo esc_url($social['url']); ?>" target="_blank" rel="noopener noreferrer" aria-label="<?php echo esc_attr($social['name']); ?>" title="<?php echo esc_attr($social['name']); ?>"><?php readfile(get_template_directory() . '/assets/images/social/' . $social['icon'] . '.svg'); ?></a><?php
    endforeach;
    ?></div><?php
}

function flynns_arcade_neon_platform_icon(string $platform): void {
    static $icons = array();
    $slug = strtolower($platform);
    if (!in_array($slug, array('switch', 'playstation', 'xbox', 'pc'), true)) return;
    if (!isset($icons[$slug])) $icons[$slug] = file_get_contents(get_template_directory() . '/assets/images/platform-' . $slug . '.svg');
    ?><span class="platform-icon platform-<?php echo esc_attr($slug); ?>" title="<?php echo esc_attr($platform); ?>"><?php echo $icons[$slug]; ?><span class="screen-reader-text"><?php echo esc_html($platform); ?></span></span><?php
}

function flynns_arcade_neon_game_card(array $game): void {
    ?>
    <article class="game-card" data-game-card data-title="<?php echo esc_attr($game['title']); ?>" data-categories="<?php echo esc_attr(implode(',', $game['categories'])); ?>">
        <a class="game-link" href="<?php echo esc_url(flynns_arcade_neon_url('game', flynns_arcade_neon_slug($game['title']))); ?>" aria-label="<?php echo esc_attr('Discover ' . $game['title']); ?>">
            <div class="game-media"><img src="<?php echo esc_url(flynns_arcade_neon_asset($game['image'])); ?>" alt="<?php echo esc_attr($game['title']); ?>" loading="lazy" decoding="async" width="768" height="432"></div>
            <div class="game-body"><h3><?php echo esc_html($game['title']); ?></h3><div class="tag-row"><?php foreach ($game['tags'] as $tag) : ?><span class="tag"><?php echo esc_html($tag); ?></span><?php endforeach; ?></div><div class="platforms"><?php foreach ($game['platforms'] as $platform) : flynns_arcade_neon_platform_icon($platform); endforeach; ?></div></div>
            <span class="game-arrow" aria-hidden="true"><?php flynns_arcade_neon_icon('arrow-right'); ?></span>
        </a>
    </article>
    <?php
}

function flynns_arcade_neon_promotions(): void {
    $slides = flynns_arcade_neon_data('site')['promotions'];
    ?>
    <section class="promotions" aria-label="Game highlights" aria-roledescription="carousel" data-promotions>
        <div class="section-inner promotion-inner">
            <div class="promotion-slides">
                <?php foreach ($slides as $index => $slide) : ?>
                    <article class="promotion-slide" data-promotion-slide<?php echo $index ? ' hidden' : ''; ?> aria-label="<?php echo esc_attr(($index + 1) . ' of ' . count($slides)); ?>">
                        <div class="promotion-copy"><p class="eyebrow"><?php echo esc_html($slide['subtitle']); ?></p><h2><?php echo esc_html($slide['title']); ?></h2><a class="arcade-button" href="<?php echo esc_url(flynns_arcade_neon_url('game', flynns_arcade_neon_slug($slide['game']))); ?>">Discover the game <?php flynns_arcade_neon_icon('arrow-right'); ?></a></div>
                        <img class="promotion-art" src="<?php echo esc_url(flynns_arcade_neon_asset($slide['image'])); ?>" alt="<?php echo esc_attr($slide['game']); ?>" loading="lazy">
                    </article>
                <?php endforeach; ?>
            </div>
            <div class="promotion-controls"><button class="icon-button" type="button" data-promotion-step="-1" aria-label="Previous highlight" title="Previous highlight"><?php flynns_arcade_neon_icon('chevron-left'); ?></button><div class="promotion-dots"><?php foreach ($slides as $index => $slide) : ?><button type="button" class="promotion-dot" data-promotion-index="<?php echo $index; ?>" aria-label="<?php echo esc_attr($slide['game']); ?>" aria-pressed="<?php echo $index ? 'false' : 'true'; ?>"></button><?php endforeach; ?></div><button class="icon-button" type="button" data-promotion-step="1" aria-label="Next highlight" title="Next highlight"><?php flynns_arcade_neon_icon('chevron-right'); ?></button></div>
        </div>
    </section>
    <?php
}
