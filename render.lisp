
(defun clamp (x a b)
  (declare int clamp x a b)
  (return (std::max a (std::min x b))))

(defun do_gamma (x)
  (declare U8 do_gamma)
  (declare F32 x)

  (coerce (clamp (* 255.0 (powf x OO_GAMMA)) 0 255) U8))

(defun pack_color_f32 (r g b))

(defun as_f32 (x))

(defun fp23_as_f32 (x))

(defun fp23_as_sf32 (x))

(defun xor_shift64 (state)
  (^= state (>> state 21))
;;  (^= state (<< state 35))
;;  (^= state (>> state  4))
  (return state))

(defun wang_hash (n))

(defun random_seed (rng seed))

(defun random_u64 (rng))

(defun random_u32 (rng))

(defun random_f32 (rng))

(defun random_sf32 (rng))

(defun += (a b)
  (declare (& vec3) += a)
  (declare (& const vec3) b)
  (+= a.x b.x)
  (+= a.y b.y)
  (+= a.z b.z)
  (return a))

;;(defun cross (a b)
;;  (declare vec3 cross)
;;  (declare (& const vec3) a b)
;;
;;  (return (vec3 (- (* a.y b.z) (* a.z b.y))
;;                (- (* a.z b.x) (* a.x b.z))
;;                (- (* a.x b.y) (* a.y b.x)))))

(defun dot (a b))

(defun length (a))

(defun normalize (a))

(defun luminance_rgb (a)
  (return (+ (* 0.212671f a.r)
             (* 0.715160f a.g)
             (* 0.072169f a.b))))

(defun map (x x0 x1 y0 y1)
  (return (+ y0 (* (- x  x0)
                   (/ (- y1 y0)
                      (- x1 x0))))))

(defun intersect_nray_sphere (ray sphere)
  (when (< s RAY_EPS)
    (return -1.0f))

  (return s))

;; PDF: pi
;;(defun sample_hemi_PSA (rng N)
;;  (declare vec3 sample_hemi_PSA)
;;
;;  (let* ((u (normalize (if (> (fabsf N.x)
;;                              (fabsf N.y))
;;                           (vec3 -N.z 0.0f N.x)
;;                           (vec3 0.0f N.z -N.y)))))
;;    (return 42)))

;;(defun sample_unit_disk (rng x y))

;;(defun rs_init (rs w h)
;;  (random_seed rs->rng 0x1337)
;;  (= rs->w w)
;;  (= rs->h h)
;;
;;  (= rs->sphere.c (vec3 0.0f 0.0f -5.0f))
;;  (= rs->sphere.r 0.64f)
;;
;;  (= rs->max_path_length 100)
;;  (= rs->spp 1))

;;(defun trace_eye_ray (rs ray))
;;
;;(defun sample_bsdf (rs trace ray w_i dw_i))
;;
;;(defun estimate_incident_radiance (rs ray))
;;
;;(defun sample_lens_local (rs dA))
;;
;;(defun sample_emitted_importance (rs x y ray dray))
;;
;;(defun estimate_measurement_integral_rgb (rs x y N)
;;  (declare vec3 estimate_measurement_integral_rgb)
;;  (declare int  x y N))
;;
;;(defun rs_render_pixel_u32 (rs x y)
;;  (declare int x y))

;;(defun rs_render_image (rs pixels))
;;
;;(defun main ()
;;  (declare int main)
;;  (return ret))
