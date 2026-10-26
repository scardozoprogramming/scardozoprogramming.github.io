<?php
/* Template Name: Flynn's Arcade: Games */
defined('ABSPATH') || exit;
get_header();
?>
<main id="main-content">
    <div class="section-inner page-heading"><h1>Games</h1></div>
<?php get_template_part('GAMES/PHP/catalog', null, array('show_heading' => false)); ?>
</main>
<?php get_footer(); ?>
