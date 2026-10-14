<?php
/** Shared accessible components. @package FlynnsArcadeNeon */
if (!defined('ABSPATH')) { exit; }

function flynns_arcade_neon_icon(string $name): void {
    ?><svg class="icon" aria-hidden="true" focusable="false"><use href="#icon-<?php echo esc_attr($name); ?>"></use></svg><?php
}

function flynns_arcade_neon_art(string $viewbox): void {
    ?><svg class="art-viewport" viewBox="<?php echo esc_attr($viewbox); ?>" preserveAspectRatio="xMidYMid slice" aria-hidden="true" focusable="false"><image href="<?php echo esc_url(flynns_arcade_neon_asset('reference-art.png')); ?>" width="1672" height="941" /></svg><?php
}

function flynns_arcade_neon_brand(bool $footer = false): void {
    ?><a class="brand reference-art<?php echo $footer ? ' footer-brand' : ''; ?>" href="#top" aria-label="Flynn's Arcade Publishing home"></a><?php
}

function flynns_arcade_neon_nav(string $class = 'site-nav'): void {
    $links = array('games' => 'Games', 'indies' => 'Indies', 'about' => 'About', 'press' => 'Press');
    ?>
    <nav class="<?php echo esc_attr($class); ?>" aria-label="<?php echo 'site-nav' === $class ? 'Primary' : 'Footer'; ?>" <?php echo 'site-nav' === $class ? 'id="site-navigation"' : ''; ?>>
        <?php foreach ($links as $id => $label) : ?>
            <a href="#<?php echo esc_attr($id); ?>" <?php if (in_array($id, array('about', 'press'), true)) : ?>data-dialog="<?php echo esc_attr($id); ?>"<?php elseif ('games' === $id && 'site-nav' === $class) : ?>class="is-active"<?php endif; ?>><?php echo esc_html__($label, 'flynns-arcade-neon'); ?></a>
        <?php endforeach; ?>
    </nav>
    <?php
}

function flynns_arcade_neon_platform_icon(string $platform): void {
    $slug = sanitize_html_class(strtolower($platform));
    $allowed = array('switch', 'playstation', 'xbox', 'pc');
    ?><span class="platform-icon platform-<?php echo esc_attr($slug); ?>" title="<?php echo esc_attr($platform); ?>"><?php
    if (in_array($slug, $allowed, true)) {
        readfile(get_template_directory() . '/assets/images/platform-' . $slug . '.svg');
    }
    ?><span class="screen-reader-text"><?php echo esc_html($platform); ?></span></span><?php
}

function flynns_arcade_neon_game_card(array $game): void {
    ?>
    <article class="game-card" data-game-card data-title="<?php echo esc_attr($game['title']); ?>" data-categories="<?php echo esc_attr(implode(',', $game['categories'])); ?>">
        <button class="game-open" type="button" data-game="<?php echo esc_attr($game['title']); ?>" data-summary="<?php echo esc_attr($game['summary']); ?>" aria-label="<?php echo esc_attr(sprintf(__('View %s', 'flynns-arcade-neon'), $game['title'])); ?>">
            <span class="game-media"><img src="<?php echo esc_url(flynns_arcade_neon_asset($game['image'])); ?>" alt="" loading="lazy" decoding="async"></span>
            <span class="game-body">
                <strong class="game-title<?php echo strlen($game['title']) > 25 ? ' long-title' : ''; ?>"><?php echo esc_html($game['title']); ?></strong>
                <span class="tag-row">
                    <?php foreach ($game['tags'] as $tag) : ?><span class="tag<?php echo 'Narrative' === $tag ? ' featured' : ''; ?>"><?php echo esc_html($tag); ?></span><?php endforeach; ?>
                </span>
                <span class="platforms">
                    <?php foreach ($game['platforms'] as $platform) : flynns_arcade_neon_platform_icon($platform); endforeach; ?>
                </span>
            </span>
        </button>
    </article>
    <?php
}

function flynns_arcade_neon_dialog_header(string $id, string $title): void {
    ?>
    <div class="dialog-header">
        <h2 id="<?php echo esc_attr($id); ?>-title"><?php echo esc_html__($title, 'flynns-arcade-neon'); ?></h2>
        <button class="icon-button dialog-close" type="button" data-close-dialog aria-label="Close" title="Close"><?php flynns_arcade_neon_icon('x'); ?></button>
    </div>
    <?php
}
