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
    return array('all' => 'All games', 'narrative' => 'Narrative', 'platform' => 'Platform', 'arcade' => 'Arcade', 'point-click' => 'Point & Click', 'puzzle' => 'Puzzle', 'rpg' => 'RPG', 'shooter' => 'Shooter');
}

function flynns_arcade_neon_editorial(): array {
    static $content;
    if (null === $content) {
        $content = json_decode(file_get_contents(get_template_directory() . '/inc/editorial.json'), true) ?: array();
    }
    return $content;
}
