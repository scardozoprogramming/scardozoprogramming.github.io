<?php
defined('ABSPATH') || exit;

function fae_sections() {
    return array('home' => 'Home', 'games' => 'Games', 'indies' => 'Indies', 'about' => 'About', 'press' => 'Press');
}

function fae_catalog() {
    static $games = null;
    if (null === $games) {
        $games = array();
        foreach ((array) json_decode(file_get_contents(FAE_PATH . 'GAMES/JSON/catalog.json'), true) as $game) {
            $games[$game['slug']] = $game;
        }
    }
    return $games;
}

function fae_asset_url($path) {
    return FAE_URL . ltrim($path, '/');
}

function fae_template_part($slug, $name = null, $args = array()) {
    $file = FAE_PATH . $slug . ($name ? '-' . $name : '') . '.php';
    if (is_file($file)) {
        include $file;
    }
}

function fae_shortcodes_in($value) {
    $found = array();
    if (is_array($value)) {
        foreach ($value as $child) {
            $found = array_merge($found, fae_shortcodes_in($child));
        }
    } elseif (is_string($value) && false !== strpos($value, '[flynns_')) {
        preg_match_all('/' . get_shortcode_regex(array('flynns_home', 'flynns_games', 'flynns_indies', 'flynns_about', 'flynns_press', 'flynns_game')) . '/s', $value, $matches, PREG_SET_ORDER);
        foreach ($matches as $match) {
            if ('[' === $match[1] && ']' === $match[6]) {
                continue;
            }
            $found[] = array('tag' => $match[2], 'attributes' => (array) shortcode_parse_atts($match[3]));
        }
    }
    return $found;
}

function fae_post_shortcodes($id) {
    $codes = fae_shortcodes_in(get_post_field('post_content', $id));
    $data = get_post_meta($id, '_elementor_data', true);
    return array_merge($codes, fae_shortcodes_in(is_string($data) ? json_decode($data, true) : $data));
}

function fae_page_map() {
    static $map = null;
    if (null === $map) {
        $map = array();
        $pages = get_posts(array('post_type' => 'page', 'post_status' => 'publish', 'numberposts' => -1, 'orderby' => 'ID', 'order' => 'ASC', 'fields' => 'ids'));
        foreach ($pages as $id) {
            foreach (fae_post_shortcodes($id) as $code) {
                $key = substr($code['tag'], 7);
                if ('game' === $key) {
                    $slug = $code['attributes']['slug'] ?? '';
                    if (!is_string($slug) || !isset(fae_catalog()[$slug])) {
                        continue;
                    }
                    $key = 'game:' . $slug;
                }
                if (!isset($map[$key])) {
                    $map[$key] = get_permalink($id);
                }
            }
        }
    }
    return $map;
}

function fae_page_url($section) {
    $map = fae_page_map();
    if (isset($map[$section])) {
        return $map[$section];
    }
    $home = $map['home'] ?? home_url('/');
    return 'home' === $section ? $home : add_query_arg('flynns_section', $section, $home);
}

function fae_game_url($slug) {
    $map = fae_page_map();
    if (!isset(fae_catalog()[$slug])) {
        return fae_page_url('games');
    }
    return $map['game:' . $slug] ?? add_query_arg('flynns_game', $slug, fae_page_url('games'));
}

function fae_url_suffix($url, $query = '', $fragment = '') {
    if ('' !== $query) {
        wp_parse_str($query, $args);
        $url = add_query_arg($args, $url);
    }
    return $url . ('' !== $fragment ? '#flynns-1-' . rawurlencode($fragment) : '');
}

function fae_enqueue_assets() {
    $styles = array('COMPARTIDO/CSS/elementor.css', 'COMPARTIDO/CSS/site.css', 'HOME/CSS/home.css', 'GAMES/CSS/catalog.css', 'GAMES/CSS/game.css', 'INDIES/CSS/indies.css', 'ABOUT/CSS/about.css', 'PRESS/CSS/press.css');
    $scripts = array('COMPARTIDO/JavaScript/site.js', 'GAMES/JavaScript/catalog.js', 'HOME/JavaScript/home.js', 'GAMES/JavaScript/games.js', 'GAMES/JavaScript/game.js', 'INDIES/JavaScript/indies.js', 'ABOUT/JavaScript/about.js', 'PRESS/JavaScript/press.js');
    $previous = array();
    foreach ($styles as $index => $file) {
        $handle = 'flynns-elementor-style-' . $index;
        wp_enqueue_style($handle, fae_asset_url($file), $previous, (string) filemtime(FAE_PATH . $file));
        $previous = array($handle);
    }
    $previous = array();
    foreach ($scripts as $index => $file) {
        $handle = 'flynns-elementor-script-' . $index;
        wp_enqueue_script($handle, fae_asset_url($file), $previous, (string) filemtime(FAE_PATH . $file), array('in_footer' => true, 'strategy' => 'defer'));
        $previous = array($handle);
    }
}

function fae_maybe_enqueue() {
    if (is_singular() && fae_post_shortcodes(get_queried_object_id())) {
        fae_enqueue_assets();
    }
}
add_action('wp_enqueue_scripts', 'fae_maybe_enqueue', 99);
add_action('elementor/preview/enqueue_styles', 'fae_enqueue_assets');
add_action('elementor/preview/enqueue_scripts', 'fae_enqueue_assets');

function fae_namespace_markup($html, $prefix) {
    $processor = new WP_HTML_Tag_Processor($html);
    while ($processor->next_tag()) {
        $id = $processor->get_attribute('id');
        if (is_string($id) && '' !== $id) {
            $processor->set_attribute('id', $prefix . $id);
        }
        foreach (array('aria-labelledby', 'aria-describedby', 'aria-controls', 'for') as $attribute) {
            $value = $processor->get_attribute($attribute);
            if (is_string($value) && '' !== $value) {
                $processor->set_attribute($attribute, implode(' ', array_map(static function ($id) use ($prefix) { return $prefix . $id; }, preg_split('/\s+/', trim($value)))));
            }
        }
        foreach (array('href', 'xlink:href') as $attribute) {
            $value = $processor->get_attribute($attribute);
            if (is_string($value) && 0 === strpos($value, '#') && strlen($value) > 1) {
                $processor->set_attribute($attribute, '#' . $prefix . substr($value, 1));
            }
        }
    }
    return $processor->get_updated_html();
}

function fae_render_shortcode($attributes = array(), $content = '', $tag = 'flynns_home') {
    static $instance = 0;
    $attributes = shortcode_atts(array('chrome' => 'yes', 'slug' => ''), $attributes, $tag);
    $view = substr($tag, 7);
    $section = 'game' === $view ? 'games' : $view;
    $slug = $attributes['slug'];
    // Only Home and Games accept alternative views while pages are being migrated.
    if (in_array($view, array('home', 'games'), true)) {
        if (isset($_GET['flynns_section'])) {
            $section = is_string($_GET['flynns_section']) ? wp_unslash($_GET['flynns_section']) : '';
            $view = isset(fae_sections()[$section]) ? $section : 'error';
        }
        if (isset($_GET['flynns_game'])) {
            $view = 'game';
            $section = 'games';
            $slug = is_string($_GET['flynns_game']) ? wp_unslash($_GET['flynns_game']) : '';
        }
    }
    $catalog = fae_catalog();
    if ('game' === $view && (!is_string($slug) || !isset($catalog[$slug]))) {
        $view = 'error';
    }
    fae_enqueue_assets();
    ob_start();
    // Saved Elementor templates can render a shortcode after wp_head has finished.
    if (did_action('wp_head') && !wp_style_is('flynns-elementor-style-7', 'done')) {
        wp_print_styles(array_map(static function ($index) { return 'flynns-elementor-style-' . $index; }, range(0, 7)));
    }
    $assets = ob_get_clean();
    $prefix = 'flynns-' . (++$instance) . '-';
    ob_start();
    fae_template_part('COMPARTIDO/PHP/icons');
    if ('no' !== $attributes['chrome']) {
        fae_template_part('COMPARTIDO/PHP/page-header', null, array('section' => $section));
    }
    if ('game' === $view) {
        include FAE_PATH . $catalog[$slug]['template'];
    } elseif (isset(fae_sections()[$view])) {
        fae_template_part(strtoupper($view) . '/PHP/index');
    } else {
        echo '<div role="region" aria-label="Page not found" class="section-inner fae-not-found"><h1>Page not found.</h1><a class="arcade-button" href="' . esc_url(fae_page_url('games')) . '">All games</a></div>';
    }
    if ('no' !== $attributes['chrome']) {
        fae_template_part('COMPARTIDO/PHP/footer-content');
    }
    $html = fae_namespace_markup(ob_get_clean(), $prefix);
    return $assets . '<div class="flynns-arcade" data-flynns-elementor data-view="' . esc_attr($view) . '" data-fragment-prefix="' . esc_attr($prefix) . '" lang="en">' . $html . '</div>';
}

function fae_register_shortcodes() {
    foreach (array_keys(fae_sections()) as $section) {
        add_shortcode('flynns_' . $section, 'fae_render_shortcode');
    }
    add_shortcode('flynns_game', 'fae_render_shortcode');
}
add_action('init', 'fae_register_shortcodes');

function fae_dynamic_element($dynamic, $data) {
    return !empty($data['settings']['shortcode']) && fae_shortcodes_in($data['settings']['shortcode']) ? true : $dynamic;
}
add_filter('elementor/element/is_dynamic_content', 'fae_dynamic_element', 10, 2);

function fae_preserve_query_route($redirect) {
    if ((isset($_GET['flynns_game']) || isset($_GET['flynns_section'])) && is_singular() && fae_post_shortcodes(get_queried_object_id())) {
        return false;
    }
    return $redirect;
}
add_filter('redirect_canonical', 'fae_preserve_query_route');
