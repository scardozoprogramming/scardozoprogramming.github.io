<?php
defined('ABSPATH') || exit;

function flynns_sections() {
    return array(
        'home'   => array('title' => 'Home',   'template' => 'HOME/PHP/index.php'),
        'games'  => array('title' => 'Games',  'template' => 'GAMES/PHP/index.php'),
        'indies' => array('title' => 'Indies', 'template' => 'INDIES/PHP/index.php'),
        'about'  => array('title' => 'About',  'template' => 'ABOUT/PHP/index.php'),
        'press'  => array('title' => 'Press',  'template' => 'PRESS/PHP/index.php'),
    );
}

function flynns_game_catalog() {
    static $games = null;
    if (null === $games) {
        $games = array();
        $file = get_theme_file_path('GAMES/JSON/catalog.json');
        $catalog = is_readable($file) ? json_decode(file_get_contents($file), true) : array();
        foreach ((array) $catalog as $game) {
            if (isset($game['slug'], $game['title'], $game['template'])) {
                $games[$game['slug']] = $game;
            }
        }
    }
    return $games;
}

function flynns_context() {
    $game = get_query_var('flynns_game', '');
    $section = get_query_var('flynns_section', '');
    $sections = flynns_sections();
    if ('' !== $game) {
        $games = flynns_game_catalog();
        if (!is_string($game) || !isset($games[$game])) {
            return array('type' => 'invalid', 'section' => 'games');
        }
        return array('type' => 'game', 'section' => 'games', 'game' => $games[$game]);
    }
    if ('' !== $section) {
        if (!is_string($section) || !isset($sections[$section])) {
            return array('type' => 'invalid', 'section' => '');
        }
        return array('type' => $section, 'section' => $section);
    }
    if (is_page()) {
        $template = get_page_template_slug(get_queried_object_id());
        foreach ($sections as $key => $definition) {
            if ($template === $definition['template']) {
                return array('type' => $key, 'section' => $key);
            }
        }
    }
    if (is_front_page()) {
        return array('type' => 'home', 'section' => 'home');
    }
    return array('type' => 'generic', 'section' => '');
}

function flynns_asset_url($path) {
    return get_theme_file_uri(ltrim($path, '/'));
}

function flynns_page_url($section) {
    static $urls = array();
    if ('home' === $section) {
        return home_url('/');
    }
    $sections = flynns_sections();
    if (!isset($sections[$section])) {
        return home_url('/');
    }
    if (!isset($urls[$section])) {
        $pages = get_posts(array(
            'post_type' => 'page', 'post_status' => 'publish', 'numberposts' => 1,
            'orderby' => 'ID', 'order' => 'ASC',
            'meta_key' => '_wp_page_template', 'meta_value' => $sections[$section]['template'],
        ));
        $urls[$section] = $pages ? get_permalink($pages[0]) : add_query_arg('flynns_section', $section, home_url('/'));
    }
    return $urls[$section];
}

function flynns_game_url($slug) {
    $games = flynns_game_catalog();
    if (!isset($games[$slug])) {
        return flynns_page_url('games');
    }
    if (get_option('permalink_structure')) {
        return home_url(user_trailingslashit('games/' . $slug));
    }
    return add_query_arg('flynns_game', $slug, home_url('/'));
}

function flynns_url_suffix($url, $query = '', $fragment = '') {
    if ('' !== $query) {
        wp_parse_str($query, $args);
        $url = add_query_arg($args, $url);
    }
    return $url . ('' !== $fragment ? '#' . rawurlencode($fragment) : '');
}

function flynns_setup() {
    add_theme_support('title-tag');
    add_theme_support('post-thumbnails');
    add_theme_support('responsive-embeds');
    add_theme_support('html5', array('search-form', 'gallery', 'caption', 'style', 'script'));
}
add_action('after_setup_theme', 'flynns_setup');

function flynns_page_templates($templates) {
    foreach (flynns_sections() as $definition) {
        $templates[$definition['template']] = "Flynn's Arcade: " . $definition['title'];
    }
    return $templates;
}
add_filter('theme_page_templates', 'flynns_page_templates');

function flynns_query_vars($vars) {
    $vars[] = 'flynns_game';
    $vars[] = 'flynns_section';
    return $vars;
}
add_filter('query_vars', 'flynns_query_vars');

function flynns_rewrite_rules() {
    add_rewrite_rule('^games/([a-z0-9-]+)/?$', 'index.php?flynns_game=$matches[1]', 'top');
}
add_action('init', 'flynns_rewrite_rules');

function flynns_activate() {
    flynns_rewrite_rules();
    flush_rewrite_rules(false);
}
add_action('after_switch_theme', 'flynns_activate');

function flynns_request_status() {
    $context = flynns_context();
    if ('invalid' === $context['type']) {
        global $wp_query;
        $wp_query->set_404();
        status_header(404);
        nocache_headers();
    }
}
add_action('template_redirect', 'flynns_request_status');

function flynns_canonical_redirect($redirect) {
    // Keep catalog filters and query-string routes on installations without pretty permalinks.
    if ('' !== get_query_var('flynns_game', '') || '' !== get_query_var('flynns_section', '')) {
        return false;
    }
    return $redirect;
}
add_filter('redirect_canonical', 'flynns_canonical_redirect');

function flynns_template($template) {
    $context = flynns_context();
    $sections = flynns_sections();
    if ('invalid' === $context['type']) {
        return get_theme_file_path('404.php');
    }
    if ('game' === $context['type']) {
        return get_theme_file_path($context['game']['template']);
    }
    if (isset($sections[$context['type']])) {
        return get_theme_file_path($sections[$context['type']]['template']);
    }
    return $template;
}
add_filter('template_include', 'flynns_template');

function flynns_title($parts) {
    $context = flynns_context();
    $sections = flynns_sections();
    if ('game' === $context['type']) {
        $parts['title'] = $context['game']['title'];
    } elseif (isset($sections[$context['type']])) {
        $parts['title'] = $sections[$context['type']]['title'];
    } else {
        return $parts;
    }
    $parts['site'] = get_bloginfo('name');
    unset($parts['tagline']);
    return $parts;
}
add_filter('document_title_parts', 'flynns_title');

function flynns_assets() {
    $type = flynns_context()['type'];
    $styles = array('COMPARTIDO/CSS/site.css', 'style.css');
    $scripts = array('COMPARTIDO/JavaScript/site.js');
    if ('home' === $type) {
        $styles[] = 'HOME/CSS/home.css';
        $styles[] = 'GAMES/CSS/catalog.css';
        $scripts[] = 'GAMES/JavaScript/catalog.js';
        $scripts[] = 'HOME/JavaScript/home.js';
    } elseif ('game' === $type) {
        $styles[] = 'GAMES/CSS/game.css';
        $scripts[] = 'GAMES/JavaScript/game.js';
    } elseif ('games' === $type) {
        $styles[] = 'GAMES/CSS/catalog.css';
        $scripts[] = 'GAMES/JavaScript/catalog.js';
        $scripts[] = 'GAMES/JavaScript/games.js';
    } elseif (in_array($type, array('indies', 'about', 'press'), true)) {
        $styles[] = strtoupper($type) . '/CSS/' . $type . '.css';
        $scripts[] = strtoupper($type) . '/JavaScript/' . $type . '.js';
    }
    $previous = array();
    foreach ($styles as $index => $file) {
        $handle = 'flynns-style-' . $index;
        wp_enqueue_style($handle, get_theme_file_uri($file), $previous, (string) filemtime(get_theme_file_path($file)));
        $previous = array($handle);
    }
    $previous = array();
    foreach ($scripts as $index => $file) {
        $handle = 'flynns-script-' . $index;
        wp_enqueue_script($handle, get_theme_file_uri($file), $previous, (string) filemtime(get_theme_file_path($file)), array('in_footer' => true, 'strategy' => 'defer'));
        $previous = array($handle);
    }
}
add_action('wp_enqueue_scripts', 'flynns_assets');
