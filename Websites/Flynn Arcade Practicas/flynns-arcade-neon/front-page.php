<?php
/** Shared page router for the independent WordPress site. @package FlynnsArcadeNeon */
if (!defined('ABSPATH')) { exit; }
$view = flynns_arcade_neon_view();
$games = flynns_arcade_neon_games();
$editorial = flynns_arcade_neon_editorial();
$site = flynns_arcade_neon_data('site');
$structure = flynns_arcade_neon_data('structure');
get_header();
?>
<main id="main-content">
    <?php require get_template_directory() . '/templates/' . $view . '.php'; ?>
</main>
<?php get_footer(); ?>
