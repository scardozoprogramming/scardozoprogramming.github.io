<?php
/** Accessible shared components. @package FlynnsArcadeNeon */
if (!defined('ABSPATH')) { exit; }

function flynns_arcade_neon_icon(string $name): void {
    ?><svg class="icon" aria-hidden="true" focusable="false"><use href="#icon-<?php echo esc_attr($name); ?>"></use></svg><?php
}

function flynns_arcade_neon_logo(): void {
    ?><span class="publisher-logo"><img src="<?php echo esc_url(flynns_arcade_neon_asset(flynns_arcade_neon_data('structure')['logo'])); ?>" width="1600" height="1000" alt="Flynn's Arcade Publishing" decoding="async"></span><?php
}

function flynns_arcade_neon_brand(): void {
    ?><a class="brand" href="<?php echo esc_url(flynns_arcade_neon_url('home')); ?>" aria-label="Flynn's Arcade home"><?php flynns_arcade_neon_logo(); ?></a><?php
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

function flynns_arcade_neon_game_card(array $game, bool $animated = false): void {
    $preview = $animated ? (flynns_arcade_neon_data('card-previews')[flynns_arcade_neon_slug($game['title'])] ?? null) : null;
    ?>
    <article class="game-card" data-game-card data-title="<?php echo esc_attr($game['title']); ?>" data-categories="<?php echo esc_attr(implode(',', $game['categories'])); ?>">
        <a class="game-link" href="<?php echo esc_url(flynns_arcade_neon_url('game', flynns_arcade_neon_slug($game['title']))); ?>" aria-label="<?php echo esc_attr('Discover ' . $game['title']); ?>">
            <div class="game-media"><img src="<?php echo esc_url(flynns_arcade_neon_asset($game['image'])); ?>"<?php if ($preview) : ?> data-game-preview="<?php echo esc_url(flynns_arcade_neon_asset($preview['image'])); ?>"<?php endif; ?> alt="<?php echo esc_attr($game['title']); ?>" loading="lazy" decoding="async" width="768" height="432"></div>
            <div class="game-body"><h3><?php echo esc_html($game['title']); ?></h3><div class="tag-row"><?php foreach ($game['tags'] as $tag) : ?><span class="tag"><?php echo esc_html($tag); ?></span><?php endforeach; ?></div><div class="platforms"><?php foreach ($game['platforms'] as $platform) : flynns_arcade_neon_platform_icon($platform); endforeach; ?></div></div>
            <span class="game-arrow" aria-hidden="true"><?php flynns_arcade_neon_icon('arrow-right'); ?></span>
        </a>
    </article>
    <?php
}

function flynns_arcade_neon_gallery_dialog(string $label): void {
    ?>
    <dialog class="image-viewer" data-gallery-dialog aria-label="<?php echo esc_attr($label); ?>">
        <div class="image-viewer-toolbar"><p data-gallery-count aria-live="polite"></p><button class="icon-button" type="button" data-gallery-close aria-label="Close photo" title="Close photo"><?php flynns_arcade_neon_icon('x'); ?></button></div>
        <div class="image-viewer-stage"><button class="icon-button" type="button" data-gallery-step="-1" aria-label="Previous photo" title="Previous photo"><?php flynns_arcade_neon_icon('chevron-left'); ?></button><img data-gallery-image alt=""><button class="icon-button" type="button" data-gallery-step="1" aria-label="Next photo" title="Next photo"><?php flynns_arcade_neon_icon('chevron-right'); ?></button></div>
    </dialog>
    <?php
}

function flynns_arcade_neon_carousel_controls(int $count, string $name): void {
    if ($count < 2) return;
    ?>
    <div class="game-carousel-controls">
        <button class="icon-button" type="button" data-carousel-step="-1" aria-label="<?php echo esc_attr('Previous ' . $name); ?>" title="<?php echo esc_attr('Previous ' . $name); ?>"><?php flynns_arcade_neon_icon('chevron-left'); ?></button>
        <span class="game-carousel-count" data-carousel-count aria-live="polite"><?php echo esc_html('1 / ' . $count); ?></span>
        <button class="icon-button" type="button" data-carousel-step="1" aria-label="<?php echo esc_attr('Next ' . $name); ?>" title="<?php echo esc_attr('Next ' . $name); ?>"><?php flynns_arcade_neon_icon('chevron-right'); ?></button>
    </div>
    <?php
}

function flynns_arcade_neon_game_blocks(array $blocks): void {
    foreach ($blocks as $block) {
        if ('list' === $block['type']) {
            echo '<ul>';
            foreach ($block['items'] as $item) echo '<li>' . esc_html($item) . '</li>';
            echo '</ul>';
        } elseif ('heading' === $block['type']) {
            echo '<h3>' . esc_html(rtrim($block['text'], ':')) . '</h3>';
        } else {
            echo '<p>' . esc_html($block['text']) . '</p>';
        }
    }
}

function flynns_arcade_neon_game_video(array $trailer, array $poster, string $title, string $id = 'trailer'): void {
    $label = 'trailer' === $id ? 'Watch trailer' : 'Watch video';
    ?>
    <div class="game-video" id="<?php echo esc_attr($id); ?>">
        <div class="game-video-frame">
            <img src="<?php echo esc_url(flynns_arcade_neon_asset($poster['image'])); ?>" alt="<?php echo esc_attr($poster['alt']); ?>" width="<?php echo (int) $poster['width']; ?>" height="<?php echo (int) $poster['height']; ?>" <?php echo 'trailer' === $id ? 'loading="lazy"' : 'fetchpriority="high"'; ?> decoding="async">
            <a class="arcade-button game-video-play" href="<?php echo esc_url($trailer['url']); ?>" target="_blank" rel="noopener noreferrer" aria-label="<?php echo esc_attr($label . ': ' . $title); ?>" data-video-id="<?php echo esc_attr($trailer['id']); ?>" data-video-start="<?php echo (int) ($trailer['start'] ?? 0); ?>" data-video-title="<?php echo esc_attr($title . ('trailer' === $id ? ' trailer' : ' video')); ?>"><?php flynns_arcade_neon_icon('play'); ?><?php echo esc_html($label); ?></a>
        </div>
        <a class="text-link game-video-external" href="<?php echo esc_url($trailer['url']); ?>" target="_blank" rel="noopener noreferrer" aria-label="<?php echo esc_attr($label . ' on YouTube: ' . $title); ?>">Watch on YouTube<?php flynns_arcade_neon_icon('external-link'); ?></a>
    </div>
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
