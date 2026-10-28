<?php
/**
 * Plugin Name: Flynn's Arcade for Elementor
 * Description: Neon pages, games and studios through Elementor shortcodes. Keeps the active theme.
 * Version: 2.0.0
 * Requires at least: 6.3
 * Requires PHP: 7.4
 * License: GPL-2.0-or-later
 * Text Domain: flynns-arcade-elementor
 */
defined('ABSPATH') || exit;
define('FAE_PATH', plugin_dir_path(__FILE__));
define('FAE_URL', plugin_dir_url(__FILE__));
require_once FAE_PATH . 'COMPARTIDO/PHP/elementor.php';
