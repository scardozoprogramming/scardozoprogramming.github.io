<?php
/** Default catalog and editorial content. @package FlynnsArcadeNeon */
if (!defined('ABSPATH')) { exit; }

function flynns_arcade_neon_asset(string $file): string {
    return get_theme_file_uri('assets/images/' . ltrim($file, '/'));
}

function flynns_arcade_neon_games(): array {
    static $games;
    if (null === $games) {
        $games = json_decode(file_get_contents(get_template_directory() . '/inc/catalog.json'), true) ?: array();
    }
    return $games;
}

function flynns_arcade_neon_filters(): array {
    return array('all' => 'All games', 'narrative' => 'Narrative', 'platform' => 'Platform', 'arcade' => 'Arcade', 'point-click' => 'Point & Click', 'puzzle' => 'Puzzle', 'rpg' => 'RPG');
}

function flynns_arcade_neon_editorial(): array {
    static $content;
    if (null === $content) {
        $content = json_decode(file_get_contents(get_template_directory() . '/inc/editorial.json'), true) ?: array();
    }
    return $content;
}

function flynns_arcade_neon_data(string $name): array {
    static $data = array();
    if (!in_array($name, array('site', 'structure'), true)) return array();
    if (!isset($data[$name])) $data[$name] = json_decode(file_get_contents(get_template_directory() . '/inc/' . $name . '.json'), true) ?: array();
    return $data[$name];
}

function flynns_arcade_neon_slug(string $text): string {
    return trim(strtolower(preg_replace('/[^a-zA-Z0-9]+/', '-', $text)), '-');
}

function flynns_arcade_neon_view(): string {
    $view = get_query_var('flynns_view', 'home');
    return in_array($view, array('home', 'games', 'indies', 'about', 'press', 'game'), true) ? $view : 'home';
}

function flynns_arcade_neon_url(string $view, string $slug = ''): string {
    if (defined('FLYNNS_STATIC_PREVIEW')) {
        return 'game' === $view ? 'game-' . $slug . '.html' : ('home' === $view ? 'preview.html' : $view . '.html');
    }
    if ('home' === $view) return home_url('/');
    return home_url('/?flynns_view=' . rawurlencode($view) . ($slug ? '&flynns_game=' . rawurlencode($slug) : ''));
}

function flynns_arcade_neon_current_game(): array {
    $slug = get_query_var('flynns_game', '');
    foreach (flynns_arcade_neon_games() as $game) { if (flynns_arcade_neon_slug($game['title']) === $slug) return $game; }
    return flynns_arcade_neon_games()[0];
}
