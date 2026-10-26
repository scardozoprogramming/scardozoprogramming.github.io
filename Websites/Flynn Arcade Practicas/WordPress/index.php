<?php
defined('ABSPATH') || exit;
get_header();
?>
<main id="main-content" class="section-inner">
<?php if (have_posts()) : ?>
    <?php while (have_posts()) : the_post(); ?>
    <article <?php post_class(); ?>>
        <div class="page-heading"><h1><?php the_title(); ?></h1></div>
        <div class="wordpress-content"><?php the_content(); wp_link_pages(); ?></div>
    </article>
    <?php endwhile; ?>
    <?php the_posts_navigation(); ?>
<?php else : ?>
    <div class="page-heading"><h1>No results <span>yet.</span></h1></div>
    <p class="wordpress-content"><a class="arcade-button" href="<?php echo esc_url(flynns_page_url('home')); ?>">Back to Home</a></p>
<?php endif; ?>
</main>
<?php get_footer(); ?>
